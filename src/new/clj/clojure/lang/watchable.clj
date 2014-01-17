(ns clojure.lang.watchable
  (:refer-clojure :only [defn])
  (:require [clojure.lang.iwatchable :refer [-add-watch -remove-watch]]))

(defn add-watch [this k f]
  (-add-watch this k f))

(defn remove-watch [this k]
  (-remove-watch this k))
