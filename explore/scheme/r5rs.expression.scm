;;; -- definition
(define definition syntax)
(define-syntax definition syntax)
;;; -- primitive express
(<variable> primitive syntax)
(quote primitive syntax)
(<quoto-datum> primitive syntax)
(<constant> primitive syntax)
(procedure-call primitive syntax)
(lambda primitive syntax)
(if primitive syntax)
(set! primitive syntax)
;; | assignment
;;
;;; -- derived express
(cond derived library-syntax)
(case derived library-syntax)
(and derived library-syntax)
(or derived library-syntax)
(let derived library-syntax)
(let* derived library-syntax)
(letrec derived library-syntax)
(begin derived library-syntax)
(do derived library-syntax)
(named-let derived library-syntax)
(delay derived library-syntax)
(quasiquote derived syntax)
;; | ` -> unquote, unqote-splice
(let-syntax derived syntax)
(letrec-syntax derived syntax)
;;syntax-rules spec
;;
