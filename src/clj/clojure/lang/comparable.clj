(ns clojure.lang.comparable
  (:refer-clojure :only [defn cond nil?])
  (:require [clojure.lang.icomparable :refer [-compare-to]]
            [clojure.lang.platform.comparable]))

(defn compare [x y]
  (cond
    (nil? x) -1
    (nil? y) 1
    :else
    (-compare-to x y)))
