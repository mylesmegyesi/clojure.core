(ns clojure.lang.seq
  (:refer-clojure :only [defn])
  (:require [clojure.lang.iseq :refer [-first -next]]))

(defn first [s]
  (-first s))

(defn next [s]
  (-next s))
