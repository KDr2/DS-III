
(defun test-add (a b c)
  (message "in fun call")
  (+ a b c))

(defadvice test-add (before test1 (a b c) activate)
  (message (format "before: %d - %d - %d" a b c))
  (setq a (* 2 a)))

(defadvice test-add (after test1 (a b c) activate)
  (message (format "after: %d - %d - %d" a b c))
  (setq ad-return-value (* a b c)))

(defadvice test-add (around test1 (a b c) activate)
  (message (format "around: %d - %d - %d" a b c))
  (setq a (* 3 a))
  ad-do-it ;; place holder for orginal function call
  (setq ad-return-value (+ a b c)))

(test-add 1 2 3)
