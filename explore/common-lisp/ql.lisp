(load "/path/to/quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:system-apropos "vecto")
(ql:quickload "vecto")
(ql:add-to-init-file)
;; search available
(ql:system-apropos "xml")
(ql:update-all-dists)
(ql:update-clint)
(ql:who-depends-on "system-name")