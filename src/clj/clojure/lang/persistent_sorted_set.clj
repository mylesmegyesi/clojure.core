(ns clojure.lang.persistent-sorted-set
  (:refer-clojure :only [apply cons declare defn defn- sorted-map sorted-map-by])
  (:require [clojure.lang.persistent-set-helper :refer [def-set make-pairs]]))

(declare make-sorted-set)

(def-set PersistentSortedSet make-sorted-set)

(defn- make-sorted-set [-sorted-map]
  (PersistentSortedSet. -sorted-map))

(defn sorted-set [& ks]
  (make-sorted-set
    (apply sorted-map (make-pairs ks))))

(defn sorted-set-by [compare-fn & ks]
  (make-sorted-set
    (apply sorted-map-by (cons compare-fn (make-pairs ks)))))
