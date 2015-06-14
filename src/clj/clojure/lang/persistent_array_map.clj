(ns clojure.lang.persistent-array-map
  (:refer-clojure :only [cond declare defn defn- deftype let loop max when if-let even? format >= <])
  (:require [clojure.lang.apersistent-map :refer [defmap]]
            [clojure.lang.array           :refer [array-copy]]
            [clojure.lang.aseq            :refer [defseq]]
            [clojure.lang.map-entry       :refer [new-map-entry]]
            [clojure.lang.exceptions      :refer [new-argument-error new-illegal-access-error]]
            [clojure.lang.key-value       :refer [platform-map-entry-type]]
            [clojure.lang.persistent-list :refer [EMPTY-LIST]]
            [clojure.lang.protocols       :refer [ICounted ILookup IAssociative IPersistentMap
                                                  IMeta IObj ISeq ISeqable
                                                  IEditableCollection ITransientCollection
                                                  ITransientAssociative
                                                  -assoc!]]
            [clojure.lang.thread          :refer [thread-reference]]
            [clojure.next                 :refer :all]))

(def ^:private hashtable-threshold 16)

(declare new-array-map
         new-array-map-seq
         make-transient-array-map)

(defn- ensure-editable [owner]
  (if (nil? owner)
    (throw (new-illegal-access-error "Transient used after persistent! call"))))

(defn- index-of [arr size value]
  (loop [i 0]
    (when (< i size)
      (if (= value (aget arr i))
        i
        (recur (+ i 2))))))

(deftype TransientArrayMap [^:volatile-mutable -length
                            ^:volatile-mutable -owner
                            -arr
                            -meta]
  ICounted
  (-count [this]
    (ensure-editable -owner)
    (/ -length 2))

  ITransientAssociative
  (-assoc! [this k v]
    (ensure-editable -owner)
    (let [idx (index-of -arr (alength -arr) k)]
      (if idx
        (do
          (when (not= (aget -arr (inc idx)) v)
            (aset -arr (inc idx) v))
          this)
        (do
          (when (>= -length (alength -arr))
            ;TODO: Become HashMap Transient
            )
          (aset -arr -length k)
          (aset -arr (inc -length) v)
          (set! -length (+ -length 2))
          this))))

  ITransientCollection
  (-conj! [this o]
    (ensure-editable -owner)
    (cond
      (instance? platform-map-entry-type o)
        (-assoc! this (key o) (val o))
      (vector? o)
        (if (= (count o) 2)
          (-assoc! this (nth o 0) (nth o 1))
          (throw (new-argument-error "Vector arg to map conj must be a pair")))
      :else
        (loop [s (seq o)]
          (if s
            (let [entry (first s)]
              (-assoc! this (key entry) (val entry))
              (recur (next s)))
            this))))

  (-persistent [this]
    (ensure-editable -owner)
    (set! -owner nil)
    (let [arr (object-array -length)]
      (array-copy -arr 0 arr 0 -length)
      (new-array-map arr (alength arr) (/ -length 2) -meta)))

  )

(defn make-transient-array-map [arr mta]
  (let [length (alength arr)
        owner (thread-reference)
        t-arr (object-array (max hashtable-threshold length))]
    (array-copy arr 0 t-arr 0 length)
    (TransientArrayMap. length owner t-arr mta)))

(defseq PersistentArrayMapSeq [arr count position]
  ICounted
  (-count [this] count)

  ISeq
  (-first [this]
    (new-map-entry (aget arr position)
                   (aget arr (inc position))))

  (-next [this]
    (new-array-map-seq arr (dec count) (+ position 2)))

  (-more [this]
    (if-let [sq (next this)] sq EMPTY-LIST)))

(defn- new-array-map-seq [arr count position]
  (when (not= 0 count)
    (PersistentArrayMapSeq. arr count position)))

(defmap PersistentArrayMap [-arr -size -count -meta]
  IAssociative
  (-assoc [this k v]
    (if-let [idx (index-of -arr -size k)] ; key exists
      (let [value-idx (inc idx)]
        (if (= v (aget -arr value-idx))
          this ; key exists and value is the same, do nothing
          (let [new-array (make-array -size)]
            (acopy -arr 0 new-array 0 -size)
            (aset new-array value-idx v)
            (new-array-map new-array -size -count -meta))))
      (let [new-size (+ -size 2)
            new-array (make-array new-size)]
        (acopy -arr 0 new-array 2 -size)
        (aset new-array 0 k)
        (aset new-array 1 v)
        (new-array-map new-array new-size (/ new-size 2) -meta))))

  (-contains-key? [this k]
    (not (nil? (index-of -arr -size k))))

  ICounted
  (-count [this] -count)

  IEditableCollection
  (-as-transient [this]
    (make-transient-array-map -arr -meta))

  ILookup
  (-lookup [this k not-found]
    (if-let [idx (index-of -arr -size k)]
      (aget -arr (inc idx))
      not-found))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (let [new-arr (make-array -size)]
      (acopy -arr 0 new-arr 0 -size)
      (new-array-map new-arr -size -count m)))

  IPersistentMap
  (-dissoc [this k]
    (if-let [idx (index-of -arr -size k)] ; key exists
      (let [new-size (- -size 2)
            new-array (make-array new-size)]
        (acopy -arr 0 new-array 0 idx)
        (acopy -arr (+ idx 2) new-array idx (- -size idx 2))
        (new-array-map new-array new-size (dec -count) -meta))
      this))

  ISeqable
  (-seq [this]
    (new-array-map-seq -arr -count 0)))

(defn new-array-map [arr size count meta]
  (PersistentArrayMap. arr size count meta))
