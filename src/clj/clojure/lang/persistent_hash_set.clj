(ns clojure.lang.persistent-hash-set
  (:refer-clojure :only [apply declare defn defn-])
  (:require [clojure.lang.apersistent-set :refer [defset]]))

(declare make-hash-set)

(defset PersistentHashSet make-hash-set)

(defn make-hash-set [-hash-map]
  (PersistentHashSet. -hash-map))

