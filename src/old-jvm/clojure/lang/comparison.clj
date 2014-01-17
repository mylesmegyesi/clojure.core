(ns clojure.lang.comparison
  (:refer-clojure :only [compare comparator]
                  :rename {compare core-compare
                           comparator core-comparator}))

(def compare core-compare)
(def comparator core-comparator)
