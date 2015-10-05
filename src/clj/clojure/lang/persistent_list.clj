(ns clojure.lang.persistent-list
  (:refer-clojure :only [declare defn defn- butlast loop satisfies?])
  (:require [clojure.next            :refer :all]
            [clojure.lang
              [aseq         :refer [defseq]]
              [deftype      :refer [deftype]]
              [equivalence  :as    equiv]
              [exceptions   :refer [new-illegal-state-error]]
              [object       :as    obj]
              [protocols    :refer [ICounted IMeta IObj IPersistentCollection -cons
                                    IPersistentList IPersistentStack ISeq ISeqable ISequential]]]))

(declare make-list)
(declare EMPTY-LIST)

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

  obj/base-object
  (equiv/equals-method [this other]
    (and
      (satisfies? ISequential other)
      (nil? (seq other)))))

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

  (-more [this] rest))

(defn- make-list [meta first rest count]
  (PersistentList. meta first rest count))

(def EMPTY-LIST (EmptyList. nil))

(defn list [& args]
  (loop [list EMPTY-LIST args args]
    (if (empty? args)
      list
      (recur (-cons list (last args)) (butlast args)))))

