(ns clojure.lang.hash
  (:refer-clojure :only [defn + * rest let loop empty? if-let first])
  (:require [clojure.lang.ihash :refer [-hash]]
            [clojure.lang.platform.hash])
  (:import [clojure.lang Util]))

(defn hash [obj]
  (-hash obj))

(defn hash-combine [hash1 hash2]
  (Util/hashCombine hash1 hash2))
