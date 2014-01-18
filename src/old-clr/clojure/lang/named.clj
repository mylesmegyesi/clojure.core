(ns clojure.lang.named
  (:refer-clojure :only [name namespace]
                  :rename {name core-name
                           namespace core-namespace}))

(def name core-name)
(def namespace core-namespace)
