(ns clojure.lang.cons
  (:refer-clojure :only [declare defn list loop])
  (:require [clojure.next           :refer :all]
            [clojure.lang
              [aseq        :refer [defseq seq-equal? seq-hash seq->array]]
              [collection  :as    coll]
              [deftype     :refer [deftype]]
              [equivalence :as    equiv]
              [exceptions  :refer [new-unsupported-error]]
              [object      :as    obj]
              [protocols   :refer [ICounted IMeta IObj
                                   ISeq ISeqable ISequential]]]))

(coll/import-collection-type)

(declare make-cons)

(deftype Cons [-meta -first -more]
  ICounted
  (-count [this]
    (inc (count -more)))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this mta]
    (make-cons mta -first -more))

  ISeq
  (-first [this] -first)

  (-next [this] (seq -more))

  (-more [this]
    (if (nil? -more)
      (list)
      -more))

  ISequential

  ISeqable
  (-seq [this] this)

  obj/base-object
  (equiv/equals-method [this other]
    (seq-equal? this other))

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
    false)

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

(defn make-cons
  ([elem s]
    (Cons. {} elem s))
  ([mta elem s]
    (Cons. mta elem s)))
