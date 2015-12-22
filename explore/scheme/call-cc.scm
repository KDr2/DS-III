;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
(define (generate-one-element-at-a-time lst)
  
  ;; Hand the next item from a-list to "return" or an end-of-list marker
  (define (control-state return)
    (for-each 
     (lambda (element)
       (set! return (call/cc
                     (lambda (resume-here)
                       ;; Grab the current continuation
                       (set! control-state resume-here)
                       (return element)))))
     lst)
    (return 'you-fell-off-the-end))
  
  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time
  (define (generator)
    (call/cc control-state)) 
  
  ;; Return the generator 
  generator)

(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))

(display (generate-digit))  ;; 0
(newline)
(display (generate-digit)) ;;1
(newline)
(display (generate-digit))
(newline)
(display (generate-digit))
(newline)
