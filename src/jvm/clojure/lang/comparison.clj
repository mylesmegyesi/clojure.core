(ns clojure.lang.comparison
  (:refer-clojure :only [defmacro extend-protocol fn])
  (:require [clojure.lang.protocols :refer [IComparable]]))

(def base-comparable Comparable)

(extend-protocol IComparable
  Object
  (-compare-to [this other]
    (.compareTo this other)))

(defmacro comparison-method [bindings & body]
  `(compareTo ~bindings ~@body))

