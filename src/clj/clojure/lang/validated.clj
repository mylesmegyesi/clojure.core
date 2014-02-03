(ns clojure.lang.validated
  (:refer-clojure :only [defn])
  (:require [clojure.lang.protocols :refer [-get-validator -set-validator!]]))

(defn get-validator [this]
  (-get-validator this))

(defn set-validator! [this validator-fn]
  (-set-validator! this validator-fn))
