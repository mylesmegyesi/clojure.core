(ns clojure.lang.persistent-hash-set
  (:refer-clojure :only [apply declare defn let when])
  (:require [clojure.lang
              [apersistent-set :refer [defset]]
              [deftype         :refer [deftype]]
              [equivalence]
              [hash]
              [object]
              [protocols :refer :all]]
            [clojure.next :refer :all]))

(declare make-hash-set
         make-transient-hash-set)

(defset PersistentHashSet make-hash-set make-transient-hash-set)

(defn make-hash-set [m]
  (PersistentHashSet. m))

(deftype TransientHashSet [^:unsynchronized-mutable -map]
  ICounted
  (-count [this]
    (count -map))

  ILookup
  (-lookup [this x]
    (get -map x))

  (-lookup [this x not-found]
    (get -map x not-found))

  IPersistentSet
  (-contains? [this x]
    (not= this (get -map x this)))

  ITransientCollection
  (-conj! [this x]
    (let [m (assoc! -map x x)]
      (when (not= m -map)
        (set! -map m)))
    this)

  (-persistent [this]
    (make-hash-set (persistent! -map)))

  ITransientSet
  (-disj! [this x]
    (let [m (dissoc! -map x)]
      (when (not= m -map)
        (set! -map m)))
    this))

(defn make-transient-hash-set [m]
  (TransientHashSet. m))

