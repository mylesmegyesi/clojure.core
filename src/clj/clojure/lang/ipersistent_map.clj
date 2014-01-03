(ns clojure.lang.ipersistent-map
  (:refer-clojure :refer [defprotocol]))

(defprotocol IPersistentMap
  (-assoc     [this k v])
  (-contains? [this k])
  (-count     [this])
  (-dissoc    [this k])
  (-lookup    [this k])
  (-seq       [this]))
