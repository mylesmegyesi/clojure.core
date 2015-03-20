(ns clojure.lang.persistent-sorted-set
  (:refer-clojure :only [apply cons declare defn defn-])
  (:require [clojure.lang.apersistent-set :refer [defset]]))

(declare make-sorted-set)

(defset PersistentSortedSet make-sorted-set)

(defn make-sorted-set [sorted-map]
  (PersistentSortedSet. sorted-map))

