(ns clojure.lang.persistent-list
  (:refer-clojure :only [declare defn defn- deftype first loop rest])
  (:require [clojure.next           :refer [cons empty? inc with-meta]]
            [clojure.lang.protocols :refer [ICounted IMeta IPersistentCollection -cons
                                            IPersistentStack ISeqable]]))


(declare make-list)

(declare EMPTY)

(deftype PersistentVector [-meta -first -rest -count]
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
    (with-meta EMPTY -meta))

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
      (recur (-cons list (first args)) (rest args)))))
