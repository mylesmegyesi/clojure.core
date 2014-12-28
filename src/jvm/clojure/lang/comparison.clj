(ns clojure.lang.comparison
  (:refer-clojure :only [defmacro defprotocol extend-protocol fn defn list update-in cons])
  (:require [clojure.lang.protocols :refer [IComparable]]))

(extend-protocol IComparable
  Object
  (-compare-to [this other]
    (.compareTo this other)))

(defn platform-compare-to-method [methods init-macro]
  (update-in methods
             ['Comparable]
             (fn [old]
               (cons
                 (list 'compareTo ['this 'other]
                       (list init-macro 'this 'other))
                 old))))
