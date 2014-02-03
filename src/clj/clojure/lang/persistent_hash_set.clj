(ns clojure.lang.persistent-hash-set
  (:refer-clojure :only [apply declare defn defn- hash-map])
  (:require [clojure.lang.apersistent-set :refer [defset make-pairs]]))

(declare new-hash-set)

(defset PersistentHashSet new-hash-set)

(defn new-hash-set [-hash-map]
  (PersistentHashSet. -hash-map))

(defn hash-set
  ([] (new-hash-set (hash-map)))
  ([& xs]
    (new-hash-set
      (apply hash-map (make-pairs xs)))))
