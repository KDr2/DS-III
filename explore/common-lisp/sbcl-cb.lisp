;; sbcl --load sbcl-cb.lisp

(defvar *callbacks* (make-hash-table))

(defmacro %defcallback (name rettype arg-names arg-types body)
  `(setf (gethash ',name *callbacks*)
         (alien-sap
          (sb-alien::alien-lambda ,rettype
              ,(mapcar (lambda (sym type)
                         (list sym  type))
                       arg-names arg-types)
            ,body))))

(defun %callback (name)
  (or (gethash name *callbacks*)
      (error "Undefined callback: ~S" name)))

(%defcallback cadd int (i j) (int int)
              (progn (format t "I'm from Lisp~%")
                     (+ i j)))

(sb-alien:load-shared-object (make-pathname
                              :name "./callback.so"))


(alien-funcall (extern-alien "set_cbf" (function void (* t)))
               (%callback 'cadd))

(alien-funcall (extern-alien "call_cbf" (function void)))

(quit)
