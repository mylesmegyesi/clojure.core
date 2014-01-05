(ns clojure.lang.logical
  (:refer-clojure :only [defn]))

(defn not
  "Returns true if x is logical false, false otherwise."
  [x]
  (if x false true))
