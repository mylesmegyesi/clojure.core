(ns clojure.lang.platform.mutable-entity
  (:refer-clojure :only [defn deftype let])
  (:require [clojure.lang.platform.imutable-entity :refer [IMutableEntity -get -set]]
            [clojure.lang.platform.mutable-array   :as    arr]))

(defn get-entity [entity]
  (-get entity))

(defn set-entity! [entity new-value]
  (-set entity new-value))

(deftype MutableEntity [-entity-arr]
  IMutableEntity
  (-get [this]
    (arr/array-get -entity-arr 0))

  (-set [this -updated-entity]
    (arr/array-set! -entity-arr 0 -updated-entity)))

(defn make-mutable-entity [value]
  (let [mutable-array (arr/make-array 1)]
    (arr/array-set! mutable-array 0 value)
    (MutableEntity. mutable-array)))
