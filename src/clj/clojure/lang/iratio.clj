(ns clojure.lang.iratio
  (:refer-clojure :only [defprotocol]))

(defprotocol IRatio
  (-numerator [this])
  (-denominator [this]))
