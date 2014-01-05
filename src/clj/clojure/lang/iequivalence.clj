(ns clojure.lang.iequivalence
  (:refer-clojure :only [defprotocol]))

(defprotocol IEquivalence
  "This protocol should not be directly used in a deftype. It should only
  be used to override an existing platform equality method."
  (-equivalent? [this other]))
