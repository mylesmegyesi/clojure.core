(ns clojure.lang.platform.comparison
  (:refer-clojure :only [defmacro defprotocol extend-protocol fn defn list -> update-in cons])
  (:require [clojure.lang.icomparable  :refer [CljComparable]]
            [clojure.lang.iequivalence :refer [IEquivalence]]
            [clojure.lang.platform.numbers :refer [number? num-equivalent? num-equal?]]))

(extend-protocol CljComparable
  Object
  (-compare-to [this other]
    (.CompareTo this other)))

(extend-protocol IEquivalence
  Object
  (-equivalent? [this other]
    (if (number? this)
      (num-equivalent? this other)
      (.Equals this other)))

  (-equal? [this other]
    (if (number? this)
      (num-equal? this other)
      (.Equals this other)))
  )

(defn platform-compare-to-method [methods init-macro]
  (update-in methods
             ['IComparable]
             (fn [old]
               (cons
                 (list 'CompareTo ['this 'other]
                       (list init-macro 'this 'other))
                 old))))

(defn platform-equals-method [methods init-macro]
  (update-in methods
             ['Object]
             (fn [old]
               (cons
                 (list 'Equals ['this 'other]
                       (list init-macro 'this 'other))
                 old))))
