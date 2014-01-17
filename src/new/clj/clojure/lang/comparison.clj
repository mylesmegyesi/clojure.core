(ns clojure.lang.comparison
  (:refer-clojure :only [cond defn defn- fn nil? number? <])
  (:require [clojure.lang.icomparable :refer [-compare-to]]
            [clojure.lang.operators   :refer [not ==]]
            [clojure.lang.platform.comparison]))

(defn comparator [predicate]
  (fn [x y]
    (cond
      (predicate x y) -1
      (predicate y x) 1
      :else 0)))

(defn- compare-numbers [x y]
  (cond
    (< x y) -1
    (< y x) 1
    :else 0))

(defn compare [x y]
  (if (== x y)
    0
    (if (not (nil? x))
      (if (nil? y)
        1
        (if (number? x)
          (compare-numbers x y)
          (-compare-to x y)))
      -1)))
