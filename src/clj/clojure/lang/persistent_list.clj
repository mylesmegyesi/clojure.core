(ns clojure.lang.persistent-list
  (:refer-clojure :only [declare defn defn- deftype loop last butlast])
  (:require [clojure.next           :refer [empty? inc with-meta =]]
            [clojure.lang.aseq      :refer [defseq]]
            [clojure.lang.protocols :refer [ICounted IMeta IPersistentCollection -cons
                                            IPersistentStack ISeq ISeqable]]))

(declare make-list)

(declare EMPTY-LIST)

(defseq PersistentList [-meta -first -rest -count]
  ICounted
  (-count [this] -count)

  IMeta
  (-meta [this] -meta)

  (-with-meta [this meta]
    (make-list meta -first -rest -count))

  IPersistentCollection
  (-cons [this x]
    (make-list -meta x this (inc -count)))

  (-empty [this]
    (with-meta EMPTY-LIST -meta))

  IPersistentStack
  (-peek [this] -first)

  (-pop [this]
    (if -rest -rest (with-meta EMPTY-LIST)))

  ISeq
  (-first [this] -first)

  (-next [this]
    (if (= -count 1) nil -rest))
)

(defn- make-list [meta first rest count]
  (PersistentList. meta first rest count))

(def EMPTY-LIST (make-list nil nil nil 0))

(defn list [& args]
  (loop [list EMPTY-LIST args args]
    (if (empty? args)
      list
      (recur (-cons list (last args)) (butlast args)))))

