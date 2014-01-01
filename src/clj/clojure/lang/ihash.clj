(ns clojure.lang.ihash
  (:refer-clojure :only [defprotocol extend-type fn]))

(defprotocol IHash
  (-hash [this]))

(extend-type nil
  IHash
  (-hash [this] 0))
