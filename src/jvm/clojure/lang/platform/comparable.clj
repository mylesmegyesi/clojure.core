(ns clojure.lang.platform.comparable
  (:refer-clojure :only [extend-protocol fn defn list update-in cons])
  (:require [clojure.lang.icomparable :refer [IComparable]]))

(defn platform-compare-to-method [methods init-macro]
  (update-in methods
             ['Comparable]
             (fn [old]
               (cons
                 (list 'compareTo ['this 'other]
                       (list init-macro 'this 'other))
                 old))))

(extend-protocol IComparable
  Object
  (-compare-to [this other]
    (.compareTo this other)))
