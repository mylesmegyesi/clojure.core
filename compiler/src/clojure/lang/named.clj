(ns clojure.lang.named
  (:refer-clojure :only [defprotocol]))

(defprotocol Named
  (name [this])
  (namespace [this]))

