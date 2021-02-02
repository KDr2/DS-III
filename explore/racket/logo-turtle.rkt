#lang racket

(require graphics/turtles)

(provide forward back
         left right
         arc)

(define (forward x) (draw x))
(define (back x) (forward (- x)))
(define (left x) (turn x))
(define (right x) (left (- x)))
(define (arc angle radius)
  (let* ([alpha (* 2 (asin (/ 1 (* 2 radius))))]
         [rangle (* pi (/ angle 180))]
         [n (abs (/ rangle alpha))]
         [astep (if (> angle 0) (- alpha) alpha)])
    (tprompt
     (move radius)
     (turn (if (> angle 0) -90 90))
     (for ([i (in-range n)])
       (draw 1)
       (turn/radians astep)))
    (turn (- angle))))
