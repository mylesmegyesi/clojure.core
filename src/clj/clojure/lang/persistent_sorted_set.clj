(ns clojure.lang.persistent-sorted-set
  (:refer-clojure :only [apply declare defn])
  (:require [clojure.lang
              [apersistent-set :refer [defset]]
              [deftype]
              [equivalence]
              [hash]
              [object]
              [protocols :refer :all]]
            [clojure.next :refer :all]))

(declare make-sorted-set)

(defset PersistentSortedSet make-sorted-set)

(defn make-sorted-set [m]
  (PersistentSortedSet. m))

