(ns clojure.lang.counted
  (:refer-clojure :only [defn extend-type fn])
  (:require [clojure.lang.protocols :refer [-count ICounted]]))

(defn count [obj]
  (-count obj))

(extend-type nil
  ICounted
  (-count [this] 0))
