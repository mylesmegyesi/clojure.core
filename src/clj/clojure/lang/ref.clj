(ns clojure.lang.ref
  (:refer-clojure :only [defn])
  (:require [clojure.lang.iref :refer [-get-validator -set-validator! -add-watch -remove-watch]]))

(defn get-validator [this]
  (-get-validator this))

(defn set-validator! [this f]
  (-set-validator! this f))

(defn add-watch [this k f]
  (-add-watch this k f))

(defn remove-watch [this k]
  (-remove-watch this k))
