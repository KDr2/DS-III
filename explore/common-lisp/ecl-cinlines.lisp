;;
;; ecl --compile ecl-clines.lisp
;; ecl --load ecl-clines.fas
;;

;; (ffi:clines "#include <stdio.h>")

(defun hello ()
  (ffi:clines "#include <stdio.h>")
  (ffi:c-inline () () () "printf(\"hello ECL FFI!\\n\");" :side-effects t))

(defun str-to-lisp ()
  (declare (optimize (debug 3)))
  ;;(ffi:clines "#include <stdio.h>")
  (ffi:c-inline () () () ; :pointer-void
                "cl_object o = c_string_to_object(\"#(1 2 3)\");
                 @(return) = o;"
                :side-effects t))

(defun multi-lines ()
  (ffi:c-inline () () () "
cl_object lisp_str = c_string_to_object(\"*package*\");
//@(return)=lisp_str;
//lisp_str=cl_make_symbol(lisp_str);
@(return)=((lisp_str)->symbol.value);"
                ))

(hello)
(format t "~s~%" (str-to-lisp))
(format t "~s~%"(multi-lines))
(quit)
