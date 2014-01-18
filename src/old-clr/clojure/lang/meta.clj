(ns clojure.lang.meta
  (:refer-clojure :only [meta reset-meta! alter-meta! with-meta]
                  :rename {meta core-meta
                           reset-meta! core-reset-meta!
                           alter-meta! core-alter-meta!
                           with-meta   core-with-meta}))

(def meta core-meta)
(def reset-meta! core-reset-meta!)
(def alter-meta! core-alter-meta!)
(def with-meta core-with-meta)
