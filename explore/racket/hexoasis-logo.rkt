#lang racket

(require racket/draw)

(define width 1024)
(define bar-height 256)
(define target (make-bitmap width width))
(define dc (new bitmap-dc% [bitmap target]))
(send dc set-smoothing 'aligned)

(send dc set-pen "white" 1 'transparent)

(let* ([side (/ width 2)]
       [half-side (/ side 2)]
       [height (* side (sqrt 3))]       
       [half-height (/ height 2)]
       [hbh (/ bar-height 2)]
       [y-offset (- side half-height)]
       [y (lambda (x) (+ x y-offset))])
  (send dc set-brush "Gold" 'solid)
  (send dc draw-polygon
        (list (cons half-side (y 0))
              (cons (* 3 half-side) (y 0))
              (cons (* 4 half-side) (y half-height))
              (cons (* 3 half-side) (y height))
              (cons half-side (y height))
              (cons 0 (y half-height))))
  (send dc set-brush "Forest Green" 'solid)
  (send dc draw-rectangle
        half-side (y 0) side (- half-height hbh))
  (send dc set-brush "CornflowerBlue" 'solid)
  (send dc draw-rectangle
        half-side (y (+ half-height hbh)) side (- half-height hbh)))


(send target save-file "hexoasis-1024.png" 'png)
