(ns clojure.lang.persistent-hash-set
  (:refer-clojure :only [apply declare defn defn- hash-map])
  (:require [clojure.lang.persistent-set-helper :refer [defset make-pairs]]))

(declare make-hash-set)

(defset PersistentHashSet make-hash-set)

(defn- make-hash-set [-hash-map]
  (PersistentHashSet. -hash-map))

(defn hash-set
  ([] (make-hash-set (hash-map)))
  ([& xs]
    (make-hash-set
      (apply hash-map (make-pairs xs)))))
