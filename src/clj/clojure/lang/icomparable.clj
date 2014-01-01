(ns clojure.lang.icomparable
  (:refer-clojure :only [defprotocol]))

(defprotocol IComparable
  (-compare-to [this other]))
