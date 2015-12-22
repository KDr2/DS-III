(ns clojure01.md-02)

;; method dispatching: protocol

(defprotocol AProtocol
  "A doc string for AProtocol abstraction"
  (bar [this a b] "bar docs")
  (baz [this a] [this a b] [this a b c] "baz docs"))


(deftype ClassA [x y z]
  AProtocol
  (bar [this a b] (+ a b x y))
  (baz [this a] (+ a x))
  (baz [this a b] (+ a b x y))
  (baz [this a b c] (+ a b x y z)))


(def
  clzb
  (reify AProtocol
    (bar [this x y] (* 2 (+ x y)))
    (baz [this x] (* 2 x))
    (baz [this x y] (* 2 (+ x y)))
    (baz [this x y z] (* 2 (+ x y z)))))


;; extend
;; extend-type
;; extend-protocol
