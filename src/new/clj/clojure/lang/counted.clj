(ns clojure.lang.counted
  (:refer-clojure :only [defn])
  (:require [clojure.lang.icounted :refer [-count]]))

(defn count [obj]
  (-count obj))
