(ns clojure01.md-01)

;;(defmulti method-name dispatch-function)
;;(defmethod method-name dispatch-value [args] body)

(derive java.util.Map ::collection)
(derive java.util.Collection ::collection)

(defmulti md-foo class)
(defmethod md-foo ::collection [c] :a-collection)
(defmethod md-foo String [s] :a-string)
