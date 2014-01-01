(ns clojure.lang.meta
  (:refer-clojure :only [defn])
  (:require [clojure.lang.imeta :refer [-meta -with-meta]]
            [clojure.lang.platform.meta]))

(defn meta [this]
  (-meta this))

(defn with-meta [this new-meta]
  (-with-meta this new-meta))
