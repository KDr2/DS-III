(ns klib
  (:require [babashka.fs :as fs]))

(defn home-path [path]
  (str (System/getenv "HOME") path))

(defn emacs-env []
  (let [bins [(home-path "/programs/emacs-nox/bin/emacs")
              (home-path "/programs/emacs.dev/bin/emacs")
              (home-path "/programs/emacs/bin/emacs")
              "/usr/bin/emacs"]
        emacs (first (filter fs/exists? bins))]
    (if emacs {:EMACS emacs} {})))

(defn show []
  (println "KLib Show!"))

(defn -main [] (println (emacs-env)))
