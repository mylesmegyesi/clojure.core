(ns clojure.lang.persistent-list
  (:refer-clojure :only [declare defn defn- deftype first loop rest])
  (:require [clojure.next           :refer [cons empty? inc]]
            [clojure.lang.protocols :refer [ICounted IPersistentCollection IPersistentStack ISeqable]]))

(declare make-list)

(deftype PersistentVector [-meta -first -rest -count]
  ICounted
  (-count [this] -count)

  IPersistentCollection
  (-cons [this x]
    (make-list -meta x this (inc -count)))

  (-empty [this])

  ISeqable
  (-seq [this] this)
)

(defn make-list [meta first rest count]
  (PersistentVector. meta first rest count))

(def ^:private EMPTY (make-list nil nil nil 0))

(defn list [& args]
  (loop [list EMPTY args args]
    (if (empty? args)
      list
      (recur (cons (first args) list) (rest args)))))
