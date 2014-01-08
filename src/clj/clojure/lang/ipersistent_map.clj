(ns clojure.lang.ipersistent-map
  (:refer-clojure :refer [defprotocol]))

(defprotocol IPersistentMap
  (-assoc     [this k v])
  (-dissoc    [this k]))
