;;
;; 1. compile the c file to a shared lib
;; 2. add the dir of the shared lib to LD_LIBRARY_PATH
;; 3. use ECL to compile this file to a fas file: ecl --compile xxx.lisp
;; 4. run the compiled file: ecl --load xxx.fas
;;

;; load the shared library
(si:load-foreign-module "callback.so")

;; define a callback, "cadd", which will be called in C
(ffi:defcallback (cadd :cdecl) :int
  ((i :int ) (j :int))
  (progn (format t "I'm from Lisp~%")
         (+ i j)))

;; call a C-function, set the callback(cadd) into the C-lib
(defun set-callback (name cb)
  (ffi:c-inline ((si:find-foreign-symbol
                  (coerce name 'base-string) :default :pointer-void 0) ; find function
                 (ffi:callback cb))
                (:pointer-void :pointer-void)
                ()
                "typedef int (*CBF)(int i, int j);
                 typedef void (*CBF_SETTER)(CBF f);
                 ((CBF_SETTER)(#0))(#1);" ; call function
                :side-effects t))

;; call a C-function, call_cbf, which calls the lisp-callback
(defun call-cfunc ()
  (ffi:c-inline ((si:find-foreign-symbol
                  (coerce "call_cbf" 'base-string) :default :pointer-void 0))
                (:pointer-void)
                ()
                "typedef void (*CF)();
                 ((CF)(#0))();"
                :side-effects t))

(set-callback "set_cbf" 'cadd)
(call-cfunc)
(quit)
