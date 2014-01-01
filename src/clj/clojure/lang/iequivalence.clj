(ns clojure.lang.iequivalence
  (:refer-clojure :only [defprotocol]))

(defprotocol IEquivalence
  (-equivalent? [this other]))
