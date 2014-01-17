(ns clojure.lang.validator
  (:refer-clojure :only [defn])
  (:require [clojure.lang.iref :refer [-get-validator -set-validator!]]))

(defn get-validator [this]
  (-get-validator this))

(defn set-validator! [this]
  (-set-validator! this))
