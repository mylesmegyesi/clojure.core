(ns clojure.lang.lookup
  (:refer-clojure :only [defn])
  (:require [clojure.lang.ilookup :refer [-lookup]]))

(defn get [o k]
  (-lookup o k))
