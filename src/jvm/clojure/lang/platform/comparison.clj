(ns clojure.lang.platform.comparison
  (:refer-clojure :only [defmacro defprotocol extend-protocol fn defn list -> update-in cons])
  (:require [clojure.lang.icomparable  :refer [IComparable]]
            [clojure.lang.iequivalence :refer [IEquivalence]]
            [clojure.lang.platform.numbers]))

(extend-protocol IComparable
  Object
  (-compare-to [this other]
    (.compareTo this other)))

(extend-protocol IEquivalence
  Object
  (-equivalent? [this other]
    (.equals this other))
  (-equal? [this other]
    (.equals this other)))

(defn platform-compare-to-method [methods init-macro]
  (update-in methods
             ['Comparable]
             (fn [old]
               (cons
                 (list 'compareTo ['this 'other]
                       (list init-macro 'this 'other))
                 old))))

(defn platform-equals-method [methods init-macro]
  (update-in methods
             ['Object]
             (fn [old]
               (cons
                 (list 'equals ['this 'other]
                       (list init-macro 'this 'other))
                 old))))
