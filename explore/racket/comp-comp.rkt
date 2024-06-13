#lang racket

(require relation/function)

(define (comp-comp f g)
  (let [[cf (curry f)]
        [cg (curry g)]]
    (uncurry (compose (lambda (x) (compose x cf)) cg))))

(define (f x) (+ x 3))
(define (g x y) (+ (* x x) (* y y)))


(displayln ((comp-comp f g) 1 2)) ;; yields 1^2 + (2+3)^2 = 26
