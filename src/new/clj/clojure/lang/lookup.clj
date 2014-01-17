(ns clojure.lang.lookup
  (:refer-clojure :only [defn])
  (:require [clojure.lang.ilookup :refer [-includes? -lookup]]))

(defn contains? [coll k]
  (-includes? coll k))

(defn get
  ([coll k] (-lookup coll k nil))
  ([coll k not-found] (-lookup coll k not-found)))
