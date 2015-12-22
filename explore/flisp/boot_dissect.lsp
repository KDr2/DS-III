;; -*- scheme -*-
(define (__start argv)
  (io.write *stdout* "test\n")
  (map (lambda (x)
         (io.write *stdout* x)
         (io.write *stdout* "\n"))
       argv)
  (exit 0))
