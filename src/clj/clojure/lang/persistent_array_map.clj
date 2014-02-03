(ns clojure.lang.persistent-array-map
  (:refer-clojure :only [declare defn defn- let + - dec loop < / inc when if-let even? format nil?])
  (:require [clojure.lang.apersistent-map     :refer [defmap]]
            [clojure.lang.array               :refer [array-get array-set! array-copy!]]
            [clojure.lang.aseq                :refer [defseq]]
            [clojure.lang.map-entry           :refer [new-map-entry]]
            [clojure.lang.platform.exceptions :refer [new-argument-error]]
            [clojure.lang.protocols           :refer [ICounted ILookup IMeta IAssociative IPersistentMap ISeq ISeqable]]
            [clojure.next                     :refer :all :exclude [+ - dec inc]]))

(declare new-array-map)

(declare new-array-map-seq)

(defseq PersistentArrayMapSeq [arr count position]
  ICounted
  (-count [this] count)

  ISeq
  (-first [this]
    (new-map-entry (array-get arr position)
                   (array-get arr (inc position))))

  (-next [this]
    (new-array-map-seq arr (dec count) (+ position 2))))

(defn- new-array-map-seq [arr count position]
  (when (not= 0 count)
    (PersistentArrayMapSeq. arr count position)))

(defn- index-of [arr size value]
  (loop [i 0]
    (when (< i size)
      (if (= value (array-get arr i))
        i
        (recur (+ i 2))))))

(defmap PersistentArrayMap [-arr -size -count -meta]
  IAssociative
  (-assoc [this k v]
    (if-let [idx (index-of -arr -size k)] ; key exists
      (let [value-idx (inc idx)]
        (if (= v (array-get -arr value-idx))
          this ; key exists and value is the same, do nothing
          (let [new-array (make-array -size)]
            (array-copy! -arr 0 new-array 0 -size)
            (array-set! new-array value-idx v)
            (new-array-map new-array -size -count -meta))))
      (let [new-size (+ -size 2)
            new-array (make-array new-size)]
        (array-copy! -arr 0 new-array 2 -size)
        (array-set! new-array 0 k)
        (array-set! new-array 1 v)
        (new-array-map new-array new-size (/ new-size 2) -meta))))

  ICounted
  (-count [this] -count)

  ILookup
  (-lookup [this k not-found]
    (if-let [idx (index-of -arr -size k)]
      (array-get -arr (inc idx))
      not-found))

  (-includes? [this k]
    (not (nil? (index-of -arr -size k))))

  IMeta
  (-meta [this] -meta)

  (-with-meta [this m]
    (let [new-arr (make-array -size)]
      (array-copy! -arr 0 new-arr 0 -size)
      (new-array-map new-arr -size -count m)))

  IPersistentMap
  (-dissoc [this k]
    (if-let [idx (index-of -arr -size k)] ; key exists
      (let [new-size (- -size 2)
            new-array (make-array new-size)]
        (array-copy! -arr 0 new-array 0 idx)
        (array-copy! -arr (+ idx 2) new-array idx (- -size idx 2))
        (new-array-map new-array new-size (dec -count) -meta))
      this))

  ISeqable
  (-seq [this]
    (new-array-map-seq -arr -count 0)))

(defn new-array-map [arr size count meta]
  (PersistentArrayMap. arr size count meta))
