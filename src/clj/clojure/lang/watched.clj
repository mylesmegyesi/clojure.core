(ns clojure.lang.watched
  (:refer-clojure :only [defn])
  (:require [clojure.lang.protocols :refer [-add-watch -remove-watch]]))

(defn add-watch [this k f]
  (-add-watch this k f))

(defn remove-watch [this k]
  (-remove-watch this k))
