(ns clojure.lang.hash
  (:refer-clojure :only [defprotocol extend-type fn]))

(defprotocol Hash
  (hash [this]))

(extend-type nil
  Hash
  (hash [this] 0))
