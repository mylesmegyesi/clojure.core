(ns clojure.lang.map-entry
  (:refer-clojure :refer [concat cond deftype defmacro defn defn- let list list* -> satisfies?])
  (:require [clojure.lang.deftype     :refer [expand-methods]]
            [clojure.lang.equivalence :refer [platform-equals-method]]
            [clojure.lang.exceptions  :refer [new-out-of-bounds-exception]]
            [clojure.lang.key-value   :refer [platform-map-entry]]
            [clojure.lang.numbers     :refer [platform-long platform-big-int platform-big-integer]]
            [clojure.lang.protocols   :refer [ICounted IIndexed ILookup IMapEntry
                                              IPersistentCollection ISeqable
                                              -key -nth -val]]
            [clojure.next             :refer :all]))

(defn- is-integer? [i]
  (or (integer? i)
      (instance? platform-long i)
      (instance? platform-big-int i)
      (instance? platform-big-integer i)))

(defn- as-vector [m]
  (vector (key m) (val m)))

(defmacro map-entry-equals? ^:private [k v other]
  `(let [other# ~other]
     (and (satisfies? IMapEntry other#)
          (= ~k (key other#))
          (= ~v (val other#)))))

(defmacro map-entry-equals?-init ^:private [this-arg other-arg]
  (list 'map-entry-equals? 'k 'v other-arg))

(defmacro defmapentry [type]
  (concat
    (list*
      'deftype type ['k 'v]

      'ICounted
      (list '-count ['this] 2)

      'IIndexed
      (list '-nth ['this 'n]
        (list 'cond
          (list 'zero? 'n) 'k
          (list '= 'n 1) 'v
          :else (list 'throw (list 'new-out-of-bounds-exception ""))))

      (list '-nth ['this 'n 'not-found]
        (list 'if (list 'or (list '= 'n 0) (list '= 'n 1))
          (list '-nth 'this 'n)
          'not-found))

      'ILookup
      (list '-lookup ['this 'k 'not-found]
        (list 'if (list 'is-integer? 'k)
          (list 'let ['i (list 'int 'k)]
            (list 'if (list 'or (list '= 'i 0) (list '= 'i 1))
              (list '-nth 'this 'k)
              'not-found))
          'not-found))

      'ISeqable
      (list '-seq ['this]
        (list 'seq (list 'as-vector 'this)))

      'IPersistentCollection
      (list '-cons ['this 'o]
        (list 'cons (list 'as-vector 'this) 'o))

      (list '-empty ['this]
        nil)

      'IMapEntry
      (list '-key ['this] 'k)
      (list '-val ['this] 'v)

      (-> {}
        (platform-equals-method 'map-entry-equals?-init)
        expand-methods))
     platform-map-entry))

(defmapentry MapEntry)

(defn new-map-entry [k v]
  (MapEntry. k v))

