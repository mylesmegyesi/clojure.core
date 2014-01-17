(ns clojure.lang.persistent-sorted-set
  (:refer-clojure :only [sorted-set sorted-set-by]
                  :rename {sorted-set core-sorted-set
                           sorted-set-by core-sorted-set-by}))

(def sorted-set    core-sorted-set)
(def sorted-set-by core-sorted-set-by)
