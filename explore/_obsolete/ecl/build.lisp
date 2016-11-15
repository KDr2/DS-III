;;
;;
;;

(defun compile-file-system (file-name
                            &optional
                              (source-ext "lisp")
                              (output-ext "o"))
  (let ((src (concatenate 'string file-name "." source-ext))
        (out (concatenate 'string file-name "." output-ext)))
    (compile-file src :output-file out :system-p t)))

(compile-file-system "ecl-cmd")
(c::build-program "ecl-cmd.exe" :lisp-files '("ecl-cmd.o"))
(ext:quit 0)
