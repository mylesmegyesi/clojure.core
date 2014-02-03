(ns clojure.lang.platform.hash-map
  (:refer-clojure :only [defmacro defn])
  (:require [clojure.lang.numbers :refer [unsafe-cast-int]]))

(defmacro ->bitnum [n]
  `(unsafe-cast-int ~n))

(defn empty-object []
  (Object.))
