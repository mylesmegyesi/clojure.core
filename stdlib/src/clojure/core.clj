(ns clojure.core)

(def = =*)

;(defn =
;  ([x] true)
;  ([x y] (clojure.lang.Util/equiv x y))
;  ([x y & more]
;   (if (clojure.lang.Util/equiv x y)
;     (if (next more)
;       (recur y (first more) (next more))
;       (clojure.lang.Util/equiv y (first more)))
