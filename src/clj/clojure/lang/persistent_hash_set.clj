(ns clojure.lang.persistent-hash-set
  (:refer-clojure :only [apply declare defn])
  (:require [clojure.lang
              [apersistent-set :refer [defset]]
              [deftype]
              [equivalence]
              [hash]
              [object]
              [protocols :refer :all]]
            [clojure.next :refer :all]))

(declare make-hash-set)

(defset PersistentHashSet make-hash-set)

(defn make-hash-set [m]
  (PersistentHashSet. m))

