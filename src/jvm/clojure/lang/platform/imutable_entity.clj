(ns clojure.lang.platform.imutable-entity
  (:refer-clojure :only [defprotocol]))

(defprotocol IMutableEntity
  (-get [this])
  (-set [this entity]))
