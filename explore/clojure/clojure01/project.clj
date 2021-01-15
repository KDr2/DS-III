(defproject clojure01 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :plugins [[cider/cider-nrepl "0.25.2"]]
  :source-paths ["src/main/clojure"]
  :target-path "target/"
  :resource-paths ["src/main/resource"]
  :test-paths ["src/test/clojure"]
  :library-path "lib"
  :main clojure01.core)
