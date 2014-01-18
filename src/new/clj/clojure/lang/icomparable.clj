(ns clojure.lang.icomparable
  (:refer-clojure :only [defprotocol]))

(defprotocol CljComparable
  "This protocol should not be directly used in a deftype. It should only
  be used to override an existing platform compare-to method."
  (-compare-to [this other]))
