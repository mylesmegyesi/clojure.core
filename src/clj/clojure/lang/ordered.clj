(ns clojure.lang.ordered
  (:refer-clojure :only [defprotocol]))

(defprotocol Ordered
  (compare-to [this other]))
