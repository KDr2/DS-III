(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "cl-cont")

(defvar cc nil)

(cl-cont:with-call/cc
    (+ 1 (cl-cont:call/cc (lambda (k)
                            (setf cc k)
                            (funcall k 2)))))
(defun f1 (cf)
  (format t "in f1~%")
  (setf cc cf)
  ;(funcall cf #(1 2 3))
  )

(cl-cont:defun/cc handler ()
  (format t "~w..." 
          ;(cl-cont:call/cc #'f1)
          1)
  (format t "bye"))

(defun h2 ()
  (format t "start...~%")
  (cl-cont:with-call/cc
    (format t "in LCC...~%")
    (format t "return from cc:~w~%" (cl-cont:call/cc #'f1)))
  (format t "end...~%"))


(with-open-file (str "~/a.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (format str "~w~%" (macroexpand-1 '(cl-cont:with-call/cc (cl-cont:call/cc #'f1) (g arg1)))))
