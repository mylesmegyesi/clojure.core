(ns clojure.lang.operators
  (:refer-clojure :only [defn nil? let cond and or complement])
  (:require [clojure.lang.platform :refer [equals type]]))

(defn ==
  ([x y]
   (let [x-nil? (nil? x)
         y-nil? (nil? y)]
     (cond
       (and x-nil? y-nil?)
       true
       (or x-nil? y-nil?)
       false
       :else
       (equals x y)))))

(defn =
  ([x y]
   (let [x-nil? (nil? x)
         y-nil? (nil? y)]
     (cond
       (and x-nil? y-nil?)
       true
       (or x-nil? y-nil?)
       false
       :else
       (and (equals (type x) (type y))
            (equals x y))))))

(def not= (complement =))

(defn ! [x] (if x false true))
(defn !! [x] (if x true false))

(def not !)
