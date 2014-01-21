(ns leiningen.deploy-deps
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.zip :as zip]
            ;;[clojure.data.xml :as xml]
            [leiningen.deploy-deps.xml :as xml]
            [clojure.data.zip.xml :as zip-xml]
            [cemerick.pomegranate.aether :as aether]
            [leiningen.core.classpath :as classpath]
            [leiningen.core.project :as project]
            [leiningen.core.main :as main]
            [leiningen.deploy :as deploy]
            [leiningen.jar :as jar]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import org.sonatype.aether.resolution.DependencyResolutionException
           org.sonatype.aether.collection.DependencyCollectionException))

;; borrowed from leiningen.deploy
(defn- abort-message [message]
  (cond (re-find #"Return code is 405" message)
        (str message "\n" "Ensure you are deploying over SSL.")
        (re-find #"Return code is 401" message)
        (str message "\n" "See `lein help deploy` for an explanation of how to"
             " specify credentials.")
        :else message))


;; Alias get-dependencies because it is private...
(def get-dependencies #'leiningen.core.classpath/get-dependencies)

(defn deps-for [project]
  (concat (keys (get-dependencies :plugins project)) (keys (get-dependencies :dependencies project))))

(defn jars-for [deps] (map (comp :file meta) deps))

(defn poms-for [jars]
  (map #(io/file (.getParent %) (str/replace (.getName %) #"\.jar" ".pom"))
       jars))

(defn files-for
  "Returns a lazy seq of dependency file maps ready for deploy."
  [project]
  (let [deps (deps-for project)
        jars (jars-for deps)
        poms (poms-for jars)]
    (main/debug "Dependencies for: " project "\n\n" deps)
    (assert (not-any? nil? (concat deps jars poms)))
    (map (fn [dep jar pom]
           {:coordinates dep
            :jar-file jar
            :pom-file pom})
         deps jars poms)))


;; Copied from leiningen.deploy source to fix detection of file uri's
;; and to provide a passphrase if needed
;; TODO: create a pull request for this!
(defn add-auth-interactively [[id settings]]
  (main/debug "auth id and settings:" (pr-str [id settings]))
  (if (or (and (:username settings) (some settings [:password :passphrase :private-key-file]))
          (.startsWith (:url settings) "file:/"))
    [id settings]
    (do
      (println "No credentials found for" id)
      (println "See `lein help deploying` for how to configure credentials.")
      (print "Username: ") (flush)
      (let [username (read-line)
            password (.readPassword (System/console) "%s"
                                    (into-array ["Password or Passphrase: "]))]
        [id (assoc settings
              :username username
              :password password
              :passphrase password)]))))


(defn- snapshot? [{:keys [jar-file]}]
  (when jar-file
    (re-find #"SNAPSHOT" (.getName jar-file))))

(defn coord? [x]
  (and (vector? x)
       (>= (count x) 2)
       (even? (count x))
       (symbol? (first x))
       (string? (second x))))

(defn update-coord
  "given a coord like [foo/bar \"1.2.3\" :classifier \"baz\"], treat the extra args as a map, and apply f. useful for things like (assoc :extension \"pom\") and (dissoc :classifier)"
  [coord f & f-args]
  {:pre [(coord? coord)]
   :post [(coord? coord)]}
  (let [[name version & extra] coord
        arg-map (apply hash-map extra)
        new-extra (apply f arg-map f-args)]
    (vec (apply concat [name version] new-extra))))

(defn pom-coord
  "Given a 'normal' coord, return the coords that will resolve the pom"
  [coord]
  (-> coord
      (update-coord dissoc :classifier)
      (update-coord assoc :extension "pom")))

(defn local-pom
  "Given a coord, return the path to the local pom for it"
  [coord repo-map]
  (let [coords [(pom-coord coord)]]
    (->> (aether/resolve-dependencies :coordinates coords :repositories repo-map)
         (filter (fn [[c deps]]
                   (= (first c) (first coord))))
         (first)
         (key)
         (meta)
         :file)))

(defn get-pom
  "Given a coordinate, return the contents of the pom.xml, as a string"
  [coord repo-map]
  {:post [%]}
  (slurp (local-pom coord repo-map)))

(defn parent-pom
  "Parse the pom, returns the coordinates of a parent pom, if any, or nil."
  [pom-str]
  (let [parent (-> pom-str
                   (xml/parse-str :namespace-aware false)
                   zip/xml-zip
                   (zip-xml/xml1->
                    :parent))]
    (when (and parent (zip/node parent))
      (let [group-id (zip-xml/xml1-> parent
                                     :groupId
                                     zip-xml/text)
            artifact-id (zip-xml/xml1-> parent
                                        :artifactId
                                        zip-xml/text)
            version (zip-xml/xml1-> parent
                                    :version
                                    zip-xml/text)]
        [(symbol (str group-id "/" artifact-id)) version]))))

(defn exists?
  "True if the dep is already in the repo"
  [coord repo-map & {:keys [retrieve]
                     :or {retrieve false}}]
  (try
    (let [resp (aether/resolve-dependencies :coordinates [coord] :repositories repo-map :retrieve true)])
    true
    (catch DependencyResolutionException e
      false)))

(defn exists-dep-map? [dep-map repo-map]
  (cond
   (:jar-file dep-map) (exists? (:coordinates dep-map) repo-map)
   (:pom-file dep-map) (exists? (pom-coord (:coordinates dep-map)) repo-map)
   :else (assert false)))

(defn resolve-parent-poms
  "Returns a list poms (paths to local files) that might need to be deployed as well"
  [coord repo-map]
  (when (parent-pom (get-pom coord repo-map))
    (loop [coord coord
           ret (list)]
      (if-let [pp (parent-pom (get-pom coord repo-map))]
        (let [pp-map {:coordinates pp
                      :pom-file (local-pom pp repo-map)}]
          (recur pp (conj ret pp-map)))
        ;; order matters, because we want higher poms to be uploaded first, but remove dups
        (distinct ret)))))

(defn deploy-deps
  "Deploy project dependencies to a remote repository.

The target repository for each dependency will be looked up in :repositories
and :deploy-repositories in project.clj:

  :repositories [[\"snapshots\" \"https://internal.repo/snapshots\"]
                 [\"releases\" \"https://internal.repo/releases\"]
                 [\"alternate\" \"https://other.server/repo\"]]

If you don't provide releases and snapshots repository names to deploy to,
either \"snapshots\" or \"releases\" will be used for each dependency depending
on the specified version. See `lein help deploying` under \"Authentication\" for
instructions on how to configure your credentials so you are not prompted on
each deploy."
  ([project releases-repository-name snapshots-repository-name]
     (with-redefs [deploy/add-auth-interactively add-auth-interactively]
       (let [releases-repo (delay (deploy/repo-for project releases-repository-name))
             snapshots-repo (delay (when snapshots-repository-name
                                     (deploy/repo-for project snapshots-repository-name)))
             all-repo-map (into {} (map (fn [[name repo]]
                                          [name (leiningen.core.user/resolve-credentials repo)]) (:repositories project)))
             all-files (files-for project)
             parent-poms (apply set/union (map (fn [dep-map]
                                                 (resolve-parent-poms (:coordinates dep-map) all-repo-map)) all-files))
             all-files (concat parent-poms all-files) ;; want parent-poms first, because they screw up dep resolution if they're not present
             ]
         (try
           (doseq [files all-files
                   :let [repo (if (snapshot? files)
                                @snapshots-repo
                                @releases-repo)
                         repo-name (if (snapshot? files)
                                     snapshots-repository-name
                                     releases-repository-name)
                         repo-map (apply hash-map repo)]]
             (when-not (exists-dep-map? files repo-map)
               (main/debug "Deploying" files "to" repo)
               (apply aether/deploy
                      (apply concat [:transfer-listener :stdout :repository [repo]] files))))
           (catch org.sonatype.aether.deployment.DeploymentException e
             (when main/*debug* (.printStackTrace e))
             (main/abort (abort-message (.getMessage e))))))))
  ([project releases-repository-name]
     (deploy-deps project releases-repository-name "snapshots"))
  ([project] (deploy-deps project "releases" "snapshots")))
