(ns clojure.lang.persistent-map
  (:refer-clojure :only [assoc dissoc]
                  :rename {assoc core-assoc
                           dissoc core-dissoc}))

(def assoc core-assoc)
(def dissoc core-dissoc)
