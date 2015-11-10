(ns clojure.lang.chunked-cons
  (:refer-clojure :only [cond declare defn loop when])
  (:require [clojure.next :refer :all]
            [clojure.lang
              [aseq            :refer [seq-equal? seq-hash seq->array]]
              [collection      :as    coll]
              [deftype         :refer [deftype]]
              [equivalence     :as    equiv]
              [exceptions      :refer [new-unsupported-error]]
              [hash            :as    hash-code]
              [object          :as    obj]
              [persistent-list :refer [EMPTY-LIST]]
              [protocols       :refer [IChunkedSeq ICounted IMeta IObj
                                       IPersistentCollection ISeq ISeqable
                                       ISequential
                                       -drop-first]]]))

(coll/import-collection-type)

(declare make-chunked-cons)

(deftype ChunkedCons [-chunk -more -meta ^:unsynchronized-mutable -hash]
  IChunkedSeq
  (-chunked-first [this] -chunk)

  (-chunked-next [this] (seq -more))

  (-chunked-more [this]
    (if (nil? -more)
      EMPTY-LIST
      -more))

  ICounted
  (-count [this]
    (loop [i 1
           sq (next this)]
      (if sq
        (recur (inc i) (next sq))
        i)))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this new-meta]
    (if (not= new-meta -meta)
      (make-chunked-cons -chunk -more new-meta)
      this))

  IPersistentCollection
  (-cons [this x]
    (cons x this))

  (-empty [this] EMPTY-LIST)

  ISeq
  (-first [this]
    (nth -chunk 0))

  (-next [this]
    (if (> (count -chunk) 1)
      (make-chunked-cons (-drop-first -chunk) -more)
      (chunk-next this)))

  (-more [this]
    (cond
      (> (count -chunk) 1)
        (make-chunked-cons (-drop-first -chunk) -more)
      (nil? -more)
        EMPTY-LIST
      :else
        -more))

  ISeqable
  (-seq [this] this)

  ISequential

  obj/base-object
  (equiv/equals-method [this other]
    (seq-equal? this other))

  (hash-code/hash-method [this]
    (when (= -hash -1)
      (set! -hash (seq-hash this)))
    -hash)

  coll/base-collection
  (coll/add-method [this other]
    (throw (new-unsupported-error)))

  (coll/add-all-method [this others]
    (throw (new-unsupported-error)))

  (coll/clear-method [this]
    (throw (new-unsupported-error)))

  (coll/contains?-method [this o]
    (loop [xs this]
      (if xs
        (if (= (first xs) o)
          true
          (recur (next xs)))
        false)))

  (coll/contains-all?-method [this os]
    (loop [xs (seq os)]
      (if xs
        (if (not (coll/contains? this (first xs)))
          false
          (recur (next xs)))
        true)))

  (coll/is-empty?-method [this]
    (nil? (seq this)))

  (coll/remove-method [this o]
    (throw (new-unsupported-error)))

  (coll/remove-all-method [this os]
    (throw (new-unsupported-error)))

  (coll/retain-all-method [this os]
    (throw (new-unsupported-error)))

  (coll/size-method [this]
    (count this))

  (coll/to-array-method [this]
    (seq->array this))

  (coll/to-array-method [this arr]
    (seq->array this arr)))

(defn make-chunked-cons
  ([ch sq]
    (ChunkedCons. ch sq nil -1))
  ([ch sq mta]
    (ChunkedCons. ch sq mta -1)))

