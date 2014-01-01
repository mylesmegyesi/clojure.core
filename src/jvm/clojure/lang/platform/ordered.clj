(ns clojure.lang.platform.ordered
  (:refer-clojure :only [extend-protocol fn defn list])
  (:require [clojure.lang.ordered :refer [Ordered]]))

(defn platform-compare-to-method []
  ['Comparable
   (list 'compareTo ['this 'other]
         (list 'compare-to 'this 'other))])

(extend-protocol Ordered
  Object
  (compare-to [this other]
    (.compareTo this other)))
