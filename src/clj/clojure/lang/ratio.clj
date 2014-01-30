(ns clojure.lang.ratio
  (:refer-clojure :only [defn deftype])
  (:require [clojure.lang.iratio :refer [IRatio -denominator -numerator]]
            [clojure.lang.object :refer [instance?]]))

(deftype Ratio [-numerator -denominator]
  IRatio
  (-numerator [this]
    -numerator)

  (-denominator [this]
    -denominator))

(defn make-ratio [numerator denominator]
  (Ratio. numerator denominator))

(defn ratio? [ratio]
  (instance? Ratio ratio))

(defn numerator [ratio]
  (-numerator ratio))

(defn denominator [ratio]
  (-denominator ratio))
