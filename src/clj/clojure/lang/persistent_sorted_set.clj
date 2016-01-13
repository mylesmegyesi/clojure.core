(ns clojure.lang.persistent-sorted-set
  (:refer-clojure :only [apply declare defn fn])
  (:require [clojure.lang
              [afn             :refer [deffn]]
              [apersistent-set :refer [defset]]
              [deftype]
              [equivalence]
              [exceptions      :refer [new-class-cast-exception]]
              [hash]
              [object]
              [protocols       :refer :all]]
            [clojure.next :refer :all]))

(declare make-sorted-set
         make-transient-sorted-set)

(defset PersistentSortedSet make-sorted-set clojure.lang.protocols.ISorted)

(defn make-sorted-set [m]
  (PersistentSortedSet. m))

