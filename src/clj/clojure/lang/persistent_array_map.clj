(ns clojure.lang.persistent-array-map
  (:refer-clojure :only [declare defn defn- deftype let + or loop < / inc when when-let if-let])
  (:require [clojure.lang.equivalence    :refer [=]]
            [clojure.lang.iassociative   :refer [IAssociative]]
            [clojure.lang.icounted       :refer [ICounted]]
            [clojure.lang.ilookup        :refer [ILookup]]
            [clojure.lang.platform.array :as    arr]))

(defn- index-of [arr size value]
  (loop [i 0]
    (when (< i size)
      (if (= value (arr/array-get arr i))
        i
        (recur (+ i 2))))))

(declare make-array-map)

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

(deftype PersistentArrayMap [arr size count]
  IAssociative
  (-assoc [this k v]
    (array-map-assoc this arr size count k v))

  ILookup
  (-lookup [this key]
    (when-let [idx (index-of arr size key)]
      (arr/array-get arr (inc idx))))

  ICounted
  (-count [this] count)
  )

(defn- make-array-map [arr size count]
  (PersistentArrayMap. arr size count))

(defn array-map [& args]
  (let [[arr size] (arr/make-array-with-items (or args []))]
    (PersistentArrayMap. arr size (/ size 2))))
