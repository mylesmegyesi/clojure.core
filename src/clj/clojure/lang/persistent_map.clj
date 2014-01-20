(ns clojure.lang.persistent-map
  (:refer-clojure :only [defn empty? first let next second])
  (:require [clojure.lang.ipersistent-map :refer [-assoc -dissoc]]))

(defn assoc
  ([m k v]
    (-assoc m k v))
  ([m k v & kvs]
    (let [persistent-map (-assoc m k v)]
      (if (empty? kvs)
        persistent-map
        (recur persistent-map (first kvs) (second kvs) (next (next kvs)))))))

(defn dissoc [m k]
  (-dissoc m k))
