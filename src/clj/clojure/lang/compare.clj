(ns clojure.lang.compare
  (:refer-clojure :only [defn cond nil?])
  (:require [clojure.lang.ordered :refer [compare-to]]))

(defn compare [x y]
  (cond
    (nil? x) -1
    (nil? y) 1
    :else
    (compare-to x y)))
