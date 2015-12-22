;; 
;; lambda/function with default args
(define (test-1 (a 1) (b 2)) (+ a b))
(princ (number->string (test-1 10)))
(newline)

;; read time evaluation [CLTL: Input/Output > PPLO > MC]
#.'(+ 1 2)

;; table library:
;;  - table table? get put! table.clone
;;  - table.values table.pairs table.keys table.invert
;;  - table.foreach  table.foldl
