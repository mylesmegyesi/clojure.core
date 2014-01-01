(ns clojure.lang.equivalence
  (:refer-clojure :only [defprotocol]))

(defprotocol Equivalence
  (equivalent? [this other]))
