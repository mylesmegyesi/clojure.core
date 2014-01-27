(ns clojure.lang.validated
  (:refer-clojure :only [defn])
  (:require [clojure.lang.ivalidatable :refer [-get-validator -set-validator!]]))

(defn get-validator [this]
  (-get-validator this))

(defn set-validator! [this validator-fn]
  (-set-validator! this validator-fn))
