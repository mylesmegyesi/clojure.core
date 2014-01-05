(ns clojure.lang.platform.comparison
  (:refer-clojure :only [defmacro defprotocol deftype extend-protocol fn defn list -> update-in cons])
  (:require [clojure.lang.icomparable      :refer [IComparable]]
            [clojure.lang.iequivalence     :refer [IEquivalence]]
            [clojure.lang.platform.numbers :refer [ops-equals no-overflow-ops]]
            [clojure.lang.platform.object  :refer [type instance?]]))

(extend-protocol IComparable
  Object
  (-compare-to [this other]
    (.compareTo this other)))

(extend-protocol IEquivalence
  Object
  (-equivalent? [this other]
    (.equals this other))

  Number
  (-equivalent? [this other]
    (if (instance? Number other)
      (-> (no-overflow-ops (type this) (type other))
        (ops-equals this other))
      false)))

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
