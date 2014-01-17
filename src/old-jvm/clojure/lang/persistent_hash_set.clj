(ns clojure.lang.persistent-hash-set
  (:refer-clojure :only [defn instance? hash-set]
                  :rename {hash-set core-hash-set}))

(def hash-set core-hash-set)

(defn hash-set? [thing]
  (instance? clojure.lang.PersistentHashSet thing))
