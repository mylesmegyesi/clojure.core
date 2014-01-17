(ns clojure.lang.hash
  (:refer-clojure :only [defn + * rest let loop empty? if-let first])
  (:require [clojure.lang.ihash :refer [-hash]]
            [clojure.lang.platform.hash]))

(defn hash [obj]
  (-hash obj))
