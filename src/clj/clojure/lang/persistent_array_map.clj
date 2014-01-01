(ns clojure.lang.persistent-array-map
  (:refer-clojure :only [defn defn- deftype let + or loop < / inc])
  (:require [clojure.lang.equivalence    :refer [=]]
            [clojure.lang.iassociative   :refer [IAssociative]]
            [clojure.lang.icounted       :refer [ICounted]]
            [clojure.lang.ilookup        :refer [ILookup]]
            [clojure.lang.platform.array :as    arr]))

(defn- index-of [arr size value]
  (loop [i 0]
    (if (< i size)
      (if (= value (arr/array-get arr i))
        i
        (recur (+ i 2)))
      -1)))

(deftype PersistentArrayMap [arr size count]
  IAssociative
  (-assoc [this k v]
    (let [new-size (+ size 2)
          new-array (arr/make-array new-size)]
      (arr/array-copy! arr 0 new-array 2 size)
      (arr/array-set! new-array 0 k)
      (arr/array-set! new-array 1 v)
      (PersistentArrayMap. new-array new-size (/ new-size 2))))

  ILookup
  (-lookup [this key]
    (let [idx (index-of arr size key)]
      (if (= -1 idx)
        nil
        (arr/array-get arr (inc idx)))))

  ICounted
  (-count [this] count)
  )

(defn array-map [& args]
  (let [[arr size] (arr/make-array-with-items (or args []))]
    (PersistentArrayMap. arr size (/ size 2))))
