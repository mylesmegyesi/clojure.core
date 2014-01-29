(ns clojure.lang.isequential
  (:refer-clojure :only [defprotocol]))

(defprotocol ISequential
  "Marker interface indicating a persistent collection of sequential items")
