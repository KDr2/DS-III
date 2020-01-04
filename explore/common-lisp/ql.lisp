;; curl -L -O https://beta.quicklisp.org/quicklisp.lisp
(load "/path/to/quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:add-to-init-file)

;; install packages
(ql:system-apropos "vecto")
(ql:quickload "vecto")

;; search available
(ql:system-apropos "xml")
(ql:update-all-dists)
(ql:update-clint)
(ql:who-depends-on "system-name")

;; load package
;; 1. load setp.lisp
;; (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                        (user-homedir-pathname))))
;;   (when (probe-file quicklisp-init)
;;     (load quicklisp-init)))
;; 2. load packate
(ql:quickload system-name)
