;;; -*- lexical-binding: t -*-

(setq lexical-binding t)

(setf xf (let ((x1 1))
          (lambda (y)
            (setf x1 (+ x1 1))
            (+ x1 y))))
(message "1=%d" (let ((x 2)) (apply xf '(2))))
(message "2=%d" (let ((x 2)) (apply xf '(2))))
(message "3=%d" (let ((x 2)) (apply xf '(2))))
