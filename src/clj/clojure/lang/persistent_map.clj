(ns clojure.lang.persistent-map
  (:refer-clojure :only [defn])
  (:require [clojure.lang.ipersistent-map :refer [-assoc -dissoc -lookup -seq -contains?]]))

(defn assoc [m k v]
  (-assoc m k v))

(defn contains? [m k]
  (-contains? m k))

(defn dissoc [m k]
  (-dissoc m k))

(defn get
  ([m k] (-lookup m k nil))
  ([m k not-found] (-lookup m k not-found)))

(defn seq [m]
  (-seq m))
