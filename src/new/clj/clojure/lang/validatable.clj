(ns clojure.lang.validatable
  (:refer-clojure :only [defn])
  (:require [clojure.lang.ivalidatable :refer [-get-validator -set-validator!]]))

(defn get-validator [this]
  (-get-validator this))

(defn set-validator! [this f]
  (-set-validator! this f))
