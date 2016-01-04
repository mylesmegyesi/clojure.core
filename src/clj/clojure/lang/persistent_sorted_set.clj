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

(defset PersistentSortedSet make-sorted-set (fn [_] (throw (new-class-cast-exception "PersistentTreeSet cannot be cast to IEditableCollection"))))

(defn make-sorted-set [m]
  (PersistentSortedSet. m))

