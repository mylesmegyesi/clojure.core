(ns clojure.lang.platform.comparable
  (:refer-clojure :only [extend-protocol fn defn list update-in cons])
  (:require [clojure.lang.icomparable :refer [IComparable]]))

(defn platform-compare-to-method [methods]
  (update-in methods
             ['Comparable]
             (fn [old]
               (cons
                 (list 'compareTo ['this 'other]
                       (list 'clojure.lang.icomparable/-compare-to 'this 'other))
                 old))))

(extend-protocol IComparable
  Object
  (-compare-to [this other]
    (.compareTo this other)))
