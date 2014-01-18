(ns clojure.lang.map-entry
  (:refer-clojure :only [defn key val]
                  :rename {key core-key
                           val core-val}))

(defn make-map-entry [key value]
  (clojure.lang.MapEntry. key value))

(def key core-key)
(def val core-val)
