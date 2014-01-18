(ns clojure.lang.counted
  (:refer-clojure :only [count]
                  :rename {count core-count}))

(def count core-count)
