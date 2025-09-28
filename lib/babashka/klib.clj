(ns klib
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell process exec]]))

(require '[clojure.java.io :as io])
(require '[clojure.string :as cljstr])

(def script-dir (-> *file* io/file
                    .getParentFile .getAbsolutePath))

(defn home-path [path]
  (str (System/getenv "HOME") path))

(defn ds3-path [path]
  (str (System/getenv "DS3_HOME") path))

(defn work-path [path]
  (str (or (System/getenv "WORK_DIR") (home-path "/work")) path))

(defn prog-path [path]
  (str (or (System/getenv "PROG_DIR") (home-path "/programs")) path))

(defn emacs-env []
  (let [bins [(prog-path "/emacs-nox/bin/emacs")
              (prog-path "/emacs.dev/bin/emacs")
              (prog-path "/emacs/bin/emacs")
              (ds3-path "/local/bin/emacs")
              "/usr/bin/emacs"]
        emacs (first (filter fs/exists? bins))]
    (if emacs {:EMACS emacs} {})))

(defn show []
  (println "KLib Show!")
  (println script-dir)
  (println *file*))


(defn -main [] (println (emacs-env)))
