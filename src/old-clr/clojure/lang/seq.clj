(ns clojure.lang.seq
  (:refer-clojure :only [first next]
                  :rename {first core-first
                           next core-next}))

(def first core-first)
(def next core-next)
