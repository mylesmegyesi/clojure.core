(ns clojure.lang.persistent-list
  (:refer-clojure :only [declare defn defn- deftype last butlast loop])
  (:require [clojure.next                     :refer [empty empty? inc with-meta =]]
            [clojure.lang.aseq                :refer [defseq]]
            [clojure.lang.platform.exceptions :refer [new-illegal-state-error]]
            [clojure.lang.protocols           :refer [ICounted IMeta IPersistentCollection -cons
                                                      IPersistentList IPersistentStack ISeq ISeqable]]))

(declare make-list)
(declare EMPTY-LIST)

(deftype EmptyList [meta]
  ICounted
  (-count [this] 0)

  IMeta
  (-meta [this] meta)

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

  ISeq
  (-first [this] nil)

  ISeqable
  (-seq [this] nil)

  (-next [this] nil)

  IPersistentList
)

(defseq PersistentList [meta first rest count]
  ICounted
  (-count [this] count)

  IMeta
  (-meta [this] meta)

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

  IPersistentList
)

(defn- make-list [meta first rest count]
  (PersistentList. meta first rest count))

(def EMPTY-LIST (EmptyList. nil))

(defn list [& args]
  (loop [list EMPTY-LIST args args]
    (if (empty? args)
      list
      (recur (-cons list (last args)) (butlast args)))))

