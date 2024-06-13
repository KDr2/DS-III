(ns clojure01.math)

(defn sum [numbers]
  (reduce + 0 numbers))

(defn prod [numbers]
  (reduce * 1 numbers))

(defn even? [x] (== (mod x 2) 0))

(defn digits
  "Split an integer to its digits."
  ([v] (digits v 10))
  ([v r]
   (map #(Integer/parseInt % r) (.split (Integer/toString v r) ""))))


;; test cases

(defn hope? [x]
  (even? (count (filter even? (digits x)))))

(nth (filter hope? (range 10000)) 2023)


(defn curry [f]
  (fn [x] (partial f x)))

(defn a [x y z] (+ x y z))

((partial a 1) 2 3)

((curry a) 1)
