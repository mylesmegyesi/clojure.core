(ns clojure.lang.persistent-map
  (:refer-clojure :only [defn])
  (:require [clojure.lang.ipersistent-map :refer [-assoc -dissoc]]))

(defn assoc [m k v]
  (-assoc m k v))

(defn dissoc [m k]
  (-dissoc m k))
