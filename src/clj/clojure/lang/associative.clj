(ns clojure.lang.associative
  (:refer-clojure :only [defn])
  (:require [clojure.lang.iassociative :refer [-assoc]]))

(defn assoc [coll k v]
  (-assoc coll k v))
