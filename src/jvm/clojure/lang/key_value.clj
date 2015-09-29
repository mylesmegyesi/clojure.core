(ns clojure.lang.key-value
  (:refer-clojure :only [extend-type defmacro fn list])
  (:require [clojure.lang.protocols :refer [IMapEntry]])
  (:import  [java.util Map Map$Entry]))

(defmacro import-map-entry-type []
  (list 'clojure.core/import '[java.util Map Map$Entry]))

(def platform-map-entry-type Map$Entry)

(defmacro key-method [bindings & body]
  `(getKey ~bindings ~@body))

(defmacro val-method [bindings & body]
  `(getValue ~bindings ~@body))

(extend-type java.util.Map$Entry
  IMapEntry
  (-key [this] (.getKey this))
  (-val [this] (.getValue this)))

