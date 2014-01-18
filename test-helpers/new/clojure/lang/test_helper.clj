(ns clojure.lang.test-helper
  (:require [clojure.lang.icomparable         :refer [CljComparable]]
            [clojure.lang.iequivalence        :refer [IEquivalence]] [clojure.lang.ihash               :refer [IHash]]
            [clojure.lang.platform.comparison :refer [platform-compare-to-method]]
            [clojure.lang.platform.object     :refer [expand-methods]]))

(deftype TenComparator []
  CljComparable
  (-compare-to [this other]
    10))

(defn new-ten-comparator []
  (TenComparator.))

(defn equal-type [equal?]
  (reify IEquivalence (-equal? [this other] equal?)))

(defn equivalent-type [equivalent?]
  (reify IEquivalence (-equivalent? [this other] equivalent?)))

(defn hashed-type [h]
  (reify IHash (-hash [this] h)))

(defmacro hashed-and-comparable-type [h i]
  (list* 'clojure.core/reify
         'clojure.lang.ihash.IHash
         (list '-hash ['this] h)

         (-> {}
           (platform-compare-to-method (list 'clojure.core/fn
                                             ['this 'other] 1))
           expand-methods)))
