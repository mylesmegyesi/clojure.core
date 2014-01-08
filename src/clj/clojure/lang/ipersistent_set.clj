(ns clojure.lang.ipersistent-set
  (:refer-clojure :only [defprotocol]))

(defprotocol IPersistentSet
  (-conj [this xs])
  (-difference [this sets])
  (-disj [this xs])
  (-intersection [this sets])
  (-union [this sets]))
