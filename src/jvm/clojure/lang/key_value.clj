(ns clojure.lang.key-value
  (:refer-clojure :only [extend-type defn fn])
  (:require [clojure.lang.protocols :refer [IMapEntry]])
  (:import  [java.util Map Map$Entry]))

(def platform-map-entry-type Map$Entry)

(def platform-map-entry
  '(java.util.Map$Entry
    (getKey   [this] (-key this))
    (getValue [this] (-val this))))

(extend-type java.util.Map$Entry
  IMapEntry
  (-key [this] (.getKey this))
  (-val [this] (.getValue this)))

