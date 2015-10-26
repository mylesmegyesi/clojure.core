(ns clojure.lang.persistent-list
  (:refer-clojure :only [declare defn defn- butlast let loop satisfies? when])
  (:require [clojure.next :refer :all]
            [clojure.lang
              [array        :as    arr]
              [aseq         :refer [defseq seq->array seq-equal? seq-hash]]
              [collection   :as    coll]
              [deftype      :refer [deftype]]
              [equivalence  :as    equiv]
              [exceptions   :refer [new-illegal-state-error
                                    new-unsupported-error]]
              [hash         :as    hash-code]
              [object       :as    obj]
              [protocols    :refer [ICounted IMeta IObj IPersistentCollection -cons
                                    IPersistentList IPersistentStack ISeq ISeqable ISequential]]]))

(declare make-list)
(declare EMPTY-LIST)

(coll/import-collection-type)

(def ^:private empty-array (arr/make-array obj/base-object 0))

(deftype EmptyList [meta]
  IPersistentList

  ICounted
  (-count [this] 0)

  IMeta
  (-meta [this] meta)

  IObj
  (-with-meta [this new-meta]
    (if (= new-meta meta)
      this
      (EmptyList. new-meta)))

  IPersistentCollection
  (-cons [this x]
    (make-list meta x this 1))

  (-empty [this] this)

  IPersistentStack
  (-peek [this] nil)

  (-pop [this]
    (throw (new-illegal-state-error "Can't pop empty list")))

  ISequential

  ISeqable
  (-seq [this] nil)

  ISeq
  (-first [this] nil)

  (-next [this] nil)

  (-more [this] this)

  coll/base-collection
  (coll/add-method [this other]
    (throw (new-unsupported-error)))

  (coll/add-all-method [this others]
    (throw (new-unsupported-error)))

  (coll/clear-method [this]
    (throw (new-unsupported-error)))

  (coll/contains?-method [this o]
    false)

  (coll/contains-all?-method [this os]
    (coll/is-empty? os))

  (coll/is-empty?-method [this]
    true)

  (coll/remove-method [this o]
    (throw (new-unsupported-error)))

  (coll/remove-all-method [this os]
    (throw (new-unsupported-error)))

  (coll/retain-all-method [this os]
    (throw (new-unsupported-error)))

  (coll/size-method [this]
    0)

  (coll/to-array-method [this]
    empty-array)

  (coll/to-array-method [this arr]
    (when (> (alength arr) 0)
      (aset arr 0 nil))
    arr)

  obj/base-object
  (equiv/equals-method [this other]
    (and
      (satisfies? ISequential other)
      (nil? (seq other))))

  (hash-code/hash-method [this]
    1))

(deftype PersistentList [-meta -first -rest -count ^:unsynchronized-mutable -hash]
  IPersistentList

  ICounted
  (-count [this] -count)

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this new-meta]
    (make-list new-meta -first -rest -count))

  IPersistentCollection
  (-cons [this x]
    (make-list -meta x this (inc -count)))

  (-empty [this]
    (with-meta EMPTY-LIST -meta))

  IPersistentStack
  (-peek [this] -first)

  (-pop [this]
    (if -rest -rest (empty this)))

  ISeq
  (-first [this] -first)

  (-next [this]
    (if (= -count 1) nil -rest))

  (-more [this] -rest)

  ISequential

  ISeqable
  (-seq [this] this)

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

(defn- make-list [meta first rest count]
  (PersistentList. meta first rest count -1))

(def EMPTY-LIST (EmptyList. nil))

(defn list [& args]
  (loop [list EMPTY-LIST args args]
    (if (empty? args)
      list
      (recur (-cons list (last args)) (butlast args)))))

