(ns clojure.lang.lazy-seq
  (:refer-clojure :only [declare defn defn- let list locking loop])
  (:require [clojure.lang
              [aseq        :refer [seq->array seq-equal?]]
              [collection  :as    coll]
              [deftype     :refer [deftype]]
              [equivalence :as    equiv]
              [exceptions  :refer [new-unsupported-error]]
              [hash        :as    hash-code]
              [object      :as    platform-object]
              [protocols   :refer [ICounted ILazySeq IMeta IObj IPending ISeq ISeqable ISequential
                                   -sval -seq -first -next -more]]]
            [clojure.next             :refer :all]))

(coll/import-collection-type)

(declare make-lazy-seq)

(deftype LazySeq [-meta
                  ^:unsynchronized-mutable f
                  ^:unsynchronized-mutable sv
                  ^:unsynchronized-mutable s
                  -seq-lock
                  -sval-lock]

  ICounted
  (-count [this]
    (loop [sq (-seq this)
           cnt 0]
      (if sq
        (recur (-next sq) (inc cnt))
        cnt)))

  ILazySeq
  (-sval [this]
    (locking -sval-lock
      (if (not (nil? f))
        (do
          (set! sv (f))
          (set! f nil)))
      (if (nil? sv)
        s
        sv)))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this new-meta]
    (make-lazy-seq new-meta f sv s))

  IPending
  (-is-realized? [this]
    (nil? f))

  ISequential

  ISeqable
  (-seq [this]
    (locking -seq-lock
      (-sval this)
      (if (not (nil? sv))
        (let [ls (loop [sls sv]
                   (if (instance? LazySeq sls)
                     (recur (-sval sls))
                     (seq sls)))]
          (set! sv nil)
          (set! s ls)))
      s))

  ISeq
  (-first [this]
    (if (nil? s)
      nil
      (-first (-seq this))))

  (-next [this]
    (if (nil? s)
      nil
      (-next (-seq this))))

  (-more [this]
    (if (nil? s)
      (list)
      (-more (-seq this))))

  platform-object/base-object
  (equiv/equals-method [this other]
    (seq-equal? this other))

  (hash-code/hash-method [this]
    (let [s (seq this)]
      (if (nil? s)
        1
        (hash-code/hash-code s))))

  coll/base-collection
  (coll/add-method [this other]
    (throw (new-unsupported-error)))

  (coll/add-all-method [this others]
    (throw (new-unsupported-error)))

  (coll/clear-method [this]
    (throw (new-unsupported-error)))

  (coll/contains?-method [this item]
    (loop [s (seq this)]
      (if s
        (if (= item (first s))
          true
          (recur (next s)))
        false)))

  (coll/contains-all?-method [this items]
    (loop [s (seq items)]
      (if s
        (if (not (coll/contains? this (first s)))
          false
          (recur (next s)))
        true)))

  (coll/is-empty?-method [this]
    (nil? (seq this)))

  (coll/remove-method [this other]
    (throw (new-unsupported-error)))

  (coll/remove-all-method [this others]
    (throw (new-unsupported-error)))

  (coll/retain-all-method [this others]
    (throw (new-unsupported-error)))

  (coll/size-method [this]
    (count this))

  (coll/to-array-method [this]
    (seq->array this))

  (coll/to-array-method [this arr]
    (seq->array this arr)))

(defn make-lazy-seq
  ([-fn]
    (make-lazy-seq nil -fn nil nil))
  ([-meta -fn -sv -seq]
    (LazySeq. -meta -fn -sv -seq (platform-object/new-base-object) (platform-object/new-base-object))))

