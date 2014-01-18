(ns clojure.lang.deref
  (:refer-clojure :only [deref]
                  :rename {deref core-deref}))

(def deref core-deref)
