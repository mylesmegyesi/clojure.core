(ns clojure.lang.map-entry
  (:refer-clojure :refer [cond defn defn- let satisfies? when])
  (:require [clojure.lang
              [aseq        :refer [seq-hash]]
              [deftype     :refer [deftype]]
              [equivalence :as    equiv]
              [exceptions  :refer [new-out-of-bounds-exception]]
              [hash        :as    hash-code]
              [key-value   :as    key-value]
              [numbers     :refer [platform-long platform-big-int platform-big-integer]]
              [object      :as    obj]
              [protocols   :refer [ICounted IIndexed ILookup IMapEntry
                                   IPersistentCollection ISeqable
                                   -key -nth -val]]]
            [clojure.next :refer :all]))

(key-value/import-map-entry-type)

(defn- is-integer? [i]
  (or (integer? i)
      (instance? platform-long i)
      (instance? platform-big-int i)
      (instance? platform-big-integer i)))

(deftype MapEntry [-k -v ^:unsynchronized-mutable -hash]
  ICounted
  (-count [this] 2)

  IIndexed
  (-nth [this n]
    (cond
      (zero? n) -k
      (= n 1) -v
      :else (throw (new-out-of-bounds-exception))))

  (-nth [this n not-found]
    (if (or (= n 0) (= n 1))
      (-nth this n)
      not-found))

  ILookup
  (-lookup [this k not-found]
    (if (is-integer? k)
      (let [i (int k)]
        (if (or (= i 0) (= i 1))
          (-nth this k)
          not-found))
      not-found))

  ISeqable
  (-seq [this]
    (seq (vector -k -v)))

  IPersistentCollection
  (-cons [this o]
    (cons (vector -k -v) o))

  (-empty [this] nil)

  IMapEntry
  (-key [this] -k)

  (-val [this] -v)

  key-value/platform-map-entry-type
  (key-value/key-method [this] -k)

  (key-value/val-method [this] -v)

  obj/base-object
  (equiv/equals-method [this other]
    (and (satisfies? IMapEntry other)
         (= -k (key other))
         (= -v (val other))))

  (hash-code/hash-method [this]
    (when (= -hash -1)
      (set! -hash (seq-hash (seq this))))
    -hash))

(defn new-map-entry [k v]
  (MapEntry. k v -1))

