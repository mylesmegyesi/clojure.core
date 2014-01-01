(ns clojure.lang.hash
  (:refer-clojure :only [defn])
  (:require [clojure.lang.ihash :refer [-hash]]
            [clojure.lang.platform.hash]))

(defn hash [obj]
  (-hash obj))
