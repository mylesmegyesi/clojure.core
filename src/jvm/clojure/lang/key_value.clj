(ns clojure.lang.key-value
  (:refer-clojure :only [])
  (:import [java.util Map Map$Entry]))

(def platform-map-entry
  '(java.util.Map$Entry
    (getKey   [this] (-key this))
    (getValue [this] (-val this))))
