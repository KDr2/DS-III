;; https://common-lisp.net/project/asdf/asdf.html

(require 'asdf)

(setf asdf:*central-registry*
      '(*default-pathname-defaults*
        #p"/path/to/lisp/systems/all/"))

(asdf:operate 'asdf:load-op 'fcgi)
