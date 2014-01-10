(ns clojure.lang.persistent-array-map
  (:refer-clojure :only [defmacro declare defn defn- deftype let + - dec or loop < / inc when when-let if-let even? format list list* and bit-and nil? ->])
  (:require [clojure.lang.counted                :refer [count]]
            [clojure.lang.icounted               :refer [ICounted]]
            [clojure.lang.ilookup                :refer [ILookup]]
            [clojure.lang.ipersistent-map        :refer [IPersistentMap]]
            [clojure.lang.iseq                   :refer [ISeq]]
            [clojure.lang.iseqable               :refer [ISeqable]]
            [clojure.lang.hash                   :refer [hash]]
            [clojure.lang.lookup                 :refer [contains? get]]
            [clojure.lang.map-entry              :refer [make-map-entry key val]]
            [clojure.lang.operators              :refer [= not not=]]
            [clojure.lang.platform.hash          :refer [platform-hash-method]]
            [clojure.lang.platform.comparison    :refer [platform-equals-method]]
            [clojure.lang.platform.enumerable    :refer [platform-enumerable-method]]
            [clojure.lang.platform.exceptions    :refer [new-argument-error]]
            [clojure.lang.platform.mutable-array :as    arr]
            [clojure.lang.platform.object        :refer [expand-methods]]
            [clojure.lang.seq                    :refer [first next seq]]))

(defn- index-of [arr size value]
  (loop [i 0]
    (when (< i size)
      (if (= value (arr/array-get arr i))
        i
        (recur (+ i 2))))))

(declare make-array-map)

(defn- array-map-contains? [arr size k]
  (not (nil? (index-of arr size k))))

(defn- array-map-dissoc [this arr size count k]
  (if-let [idx (index-of arr size k)] ; key exists
    (let [new-size (- size 2)
          new-array (arr/make-array new-size)]
      (arr/array-copy! arr 0 new-array 0 idx)
      (arr/array-copy! arr (+ idx 2) new-array idx (- size idx 2))
      (make-array-map new-array new-size (dec count)))
    this))

(defn- array-map-assoc [this arr size count k v]
  (if-let [idx (index-of arr size k)] ; key exists
    (let [value-idx (inc idx)]
      (if (= v (arr/array-get arr value-idx))
        this ; key exists and value is the same, do nothing
        (let [new-array (arr/make-array size)]
          (arr/array-copy! arr 0 new-array 0 size)
          (arr/array-set! new-array value-idx v)
          (make-array-map new-array size count))))
    (let [new-size (+ size 2)
          new-array (arr/make-array new-size)]
      (arr/array-copy! arr 0 new-array 2 size)
      (arr/array-set! new-array 0 k)
      (arr/array-set! new-array 1 v)
      (make-array-map new-array new-size (/ new-size 2)))))

(defn- array-map-lookup [arr size key not-found]
  (if-let [idx (index-of arr size key)]
    (arr/array-get arr (inc idx))
    not-found))

(defn- array-map-equals? [-seq -count other]
  (if (= -count (count other))
    (loop [this-seq -seq]
      (if this-seq
        (let [first-entry (first this-seq)
              k (key first-entry)
              v (val first-entry)]
          (if (and (contains? other k)
                   (= (get other k) v))
            (recur (next this-seq))
            false))
        true))
    false))

(defmacro array-map-equals?-init
  {:private true}
  [this-arg other-arg]
  (list 'array-map-equals? '-seq '-count other-arg))

(defmacro array-map-hash
  {:private true}
  [-seq]
  `(loop [entries# ~-seq
          acc#     0]
     (if entries#
       (let [entry# (first entries#)]
         (recur
           (next entries#)
           (+ acc# (bit-and (hash (key entry#))
                            (hash (val entry#))))))
       acc#)))

(declare make-array-map-seq)

(deftype PersistentArrayMapSeq [first-entry arr count next-count position]
  ICounted
  (-count [this] count)

  ISeq
  (-first [this] first-entry)

  (-next [this]
    (make-array-map-seq arr next-count (dec next-count) position))

  )

(defn- make-array-map-seq [arr count next-count position]
  (when (not= 0 count)
    (PersistentArrayMapSeq.
      (make-map-entry (arr/array-get arr position)
                      (arr/array-get arr (inc position)))
      arr count next-count (+ position 2))))

(defmacro array-map-hash-init
  {:private true}
  [_]
  (list 'array-map-hash '-seq))

(def platform-array-map-methods
  ^{:private true}
  (-> {}
    (platform-hash-method 'array-map-hash-init)
    ;platform-show-method
    platform-enumerable-method
    (platform-equals-method 'array-map-equals?-init)
    expand-methods))

(defmacro defpersistentarraymap [type]
  (list*
    'deftype type ['-arr '-size '-count '-seq]

    'ICounted
    (list '-count ['this] '-count)

    'ILookup
    (list '-lookup ['this 'k 'not-found]
          (list 'array-map-lookup '-arr '-size 'k 'not-found))

    (list '-includes? ['this 'k]
          (list 'array-map-contains? '-arr '-size 'k))

    'IPersistentMap
    (list '-assoc ['this 'k 'v]
          (list 'array-map-assoc 'this '-arr '-size '-count 'k 'v))

    (list '-dissoc ['this 'k]
          (list 'array-map-dissoc 'this '-arr '-size '-count 'k))

    'ISeqable
    (list '-seq ['this] '-seq)

    platform-array-map-methods))

(defpersistentarraymap PersistentArrayMap)

(defn- make-array-map [arr size count]
  (PersistentArrayMap. arr size count (make-array-map-seq arr count (dec count) 0)))

(defn array-map [& args]
  (let [[arr size] (arr/make-array-with-items (or args []))]
    (if (even? size)
      (make-array-map arr size (/ size 2))
      (throw (new-argument-error
               (format "PersistenArrayMap can only be created with even number of arguements: %s arguements given"
                       size))))))
