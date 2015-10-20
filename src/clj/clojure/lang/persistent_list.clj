(ns clojure.lang.persistent-list
  (:refer-clojure :only [declare defn defn- butlast let loop satisfies? when])
  (:require [clojure.next :refer :all]
            [clojure.lang
              [array        :as    arr]
              [aseq         :refer [defseq]]
              [collection   :as    coll]
              [deftype      :refer [deftype]]
              [equivalence  :as    equiv]
              [exceptions   :refer [new-illegal-state-error
                                    new-unsupported-error]]
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
    (new-unsupported-error))

  (coll/add-all-method [this others]
    (new-unsupported-error))

  (coll/clear-method [this]
    (new-unsupported-error))

  (coll/contains?-method [this o]
    false)

  (coll/contains-all?-method [this os]
    (coll/is-empty? os))

  (coll/is-empty?-method [this]
    true)

  (coll/remove-method [this o]
    (new-unsupported-error))

  (coll/remove-all-method [this os]
    (new-unsupported-error))

  (coll/retain-all-method [this os]
    (new-unsupported-error))

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
      (nil? (seq other)))))

(defn ^:private fill-array [arr sq]
  (loop [s sq
         i 0]
    (if s
      (do
        (aset arr i (first s))
        (recur (next s) (inc i)))
      arr)))

(defseq PersistentList [meta first rest count]
  IPersistentList

  ICounted
  (-count [this] count)

  IMeta
  (-meta [this] meta)

  IObj
  (-with-meta [this new-meta]
    (make-list new-meta first rest count))

  IPersistentCollection
  (-cons [this x]
    (make-list meta x this (inc count)))

  (-empty [this]
    (with-meta EMPTY-LIST meta))

  IPersistentStack
  (-peek [this] first)

  (-pop [this]
    (if rest rest (empty this)))

  ISeq
  (-first [this] first)

  (-next [this]
    (if (= count 1) nil rest))

  (-more [this] rest)

  coll/base-collection
  (coll/add-method [this other]
    (new-unsupported-error))

  (coll/add-all-method [this others]
    (new-unsupported-error))

  (coll/clear-method [this]
    (new-unsupported-error))

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
    (new-unsupported-error))

  (coll/remove-all-method [this os]
    (new-unsupported-error))

  (coll/retain-all-method [this os]
    (new-unsupported-error))

  (coll/size-method [this]
    (count this))

  (coll/to-array-method [this]
    (object-array this))

  (coll/to-array-method [this arr]
    (let [len (count this)]
      (if (> len (alength arr))
        (let [new-arr (make-array (arr/get-array-type arr) len)]
          (fill-array new-arr this)
          new-arr)
        (let [new-arr (aclone arr)]
          (fill-array new-arr this)
          (aset new-arr len nil)
          new-arr)))))

(defn- make-list [meta first rest count]
  (PersistentList. meta first rest count))

(def EMPTY-LIST (EmptyList. nil))

(defn list [& args]
  (loop [list EMPTY-LIST args args]
    (if (empty? args)
      list
      (recur (-cons list (last args)) (butlast args)))))

