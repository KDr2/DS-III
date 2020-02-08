#|
To create a program called ls.exe type the following lines
from a lisp prompt:

(compile-file "ecl-cmd.lisp" :output-file "ecl-cmd.o" :system-p t)
(c::build-program "ecl-cmd.exe" :lisp-files '("ecl-cmd.o"))

See: https://common-lisp.net/project/ecl/static/manual/rn01re61.html

|#

#-:ecl
(progn
  (format t "ECL is the only supported implementation~C" #\Newline)
  (quit))

(setq ext:*help-message* "
ecl-cmd.exe usage:
	ecl-cmd.exe arg1 arg2 ...
")

(defun default-behaviour (args)
  #+nil
  (if (listp args)
      (print "list!")
      (print (type-of args)))
  (format t "~S~%" args))

(defconstant +app-rules+
  '(("--help" 0 (progn (princ ext:*help-message* *standard-output*) (ext:quit 0)))
    ("-?" 0 (progn (princ ext:*help-message* *standard-output*) (ext:quit 0)))
    ("*DEFAULT*" 1 (default-behaviour 1) :stop)))

(let ((ext:*lisp-init-file-list* NIL)) ; No initialization files
  (handler-case (ext:process-command-args :rules +app-rules+)
    (error (c)
      (princ ext:*help-message* *error-output*)
      (ext:quit 2))))

(if (<= (length (si::command-args)) 1)
    (progn (princ ext:*help-message* *standard-output*)
           (ext:quit 1)))
(ext:quit 0)
