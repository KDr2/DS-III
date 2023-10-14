(ns klib
  (:require [babashka.fs :as fs]
            [babashka.process :refer [shell process exec]]))

(require '[clojure.java.io :as io])

(def script-dir (-> *file* io/file
                    .getParentFile .getAbsolutePath))

(defn home-path [path]
  (str (System/getenv "HOME") path))

(defn nix-shell-cmd [name]
  (str "nix-shell " script-dir "/../nix/" name "-ns.nix"))

(defn nix-shell [name]
  (if (= name "list")
    (doall (->> (.listFiles (io/file (str script-dir "/../nix/")))
                (map #(.getName %)) (filter #(.endsWith % "-ns.nix"))
                (map #(.replace % "-ns.nix" "")) (map println)))
    (shell (nix-shell-cmd name))))

(defn emacs-env []
  (let [bins [(home-path "/programs/emacs-nox/bin/emacs")
              (home-path "/programs/emacs.dev/bin/emacs")
              (home-path "/programs/emacs/bin/emacs")
              "/usr/bin/emacs"]
        emacs (first (filter fs/exists? bins))
        not-in-nix-shell (empty? (System/getenv "IN_NIX_SHELL"))]
    (if (and not-in-nix-shell emacs) {:EMACS emacs} {})))

(defn show []
  (println "KLib Show!")
  (println script-dir)
  (println *file*))


(defn -main [] (println (emacs-env)))
