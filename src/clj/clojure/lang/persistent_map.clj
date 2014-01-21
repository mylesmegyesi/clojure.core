(ns clojure.lang.persistent-map
  (:refer-clojure :only [defn empty? first let next second])
  (:require [clojure.lang.ipersistent-map :refer [-assoc -dissoc]]))

(defn assoc
  ([m k v]
    (-assoc m k v))
  ([m k v & kvs]
    (let [persistent-map (-assoc m k v)]
      (if kvs
        (recur persistent-map (first kvs) (second kvs) (next (next kvs)))
        persistent-map))))

(defn dissoc
  ([m] m)
  ([m k] (-dissoc m k))
  ([m k & ks]
   (let [persistent-map (-dissoc m k)]
     (if ks
       (recur persistent-map (first ks) (next ks))
       persistent-map))))
