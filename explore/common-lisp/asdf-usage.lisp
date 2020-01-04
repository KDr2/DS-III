;; /home/kdr2/work/hacking/lisp/asdf/all

(require 'asdf)

(setf asdf:*central-registry*
      '(*default-pathname-defaults*
        #p"/home/kdr2/work/hacking/lisp/asdf/all/"))

(asdf:operate 'asdf:load-op 'fcgi)
