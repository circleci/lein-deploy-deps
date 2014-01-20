(defproject lein-deploy-deps "0.1.2"
  :description "Deploy project dependencies to a remote repository"
  :url "https://github.com/neatonk/lein-deploy-deps"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :eval-in-leiningen true
  :dependencies [[org.clojure/data.zip "0.1.1" :exclusions [org.clojure/clojure]]
                 ;; currently ignored because lein depends on 0.0.3
                 [org.clojure/data.xml "0.0.7"]])
