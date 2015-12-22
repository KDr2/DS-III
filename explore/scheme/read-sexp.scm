#!/usr/bin/env guile
!#
(define data
  (let ((f (open-file (cadr (program-arguments)) "r")))
    (let loop ((exp (read f))
               (ret '()))
      (if (eof-object? exp)
          (begin
            (close f)
            ret)
          (loop (read f) (append ret (list exp)))))))

;;(display data)
(for-each (lambda (i)
            (display "   - ")
            (display (car i))
            (newline))
          data)
(newline)
