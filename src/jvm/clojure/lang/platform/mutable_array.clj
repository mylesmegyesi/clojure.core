(ns clojure.lang.platform.mutable-array
  (:refer-clojure :only [defn let loop inc prn int])
  (:require [clojure.lang.platform.equivalence :refer [->int]])
  (:import [java.lang.reflect Array]
           [java.util ArrayList]))

(defn make-array [size]
  (Array/newInstance Object (int size)))

(defn make-array-with-items [iterable]
  (let [it (.iterator iterable)
        arr-list (ArrayList.)]
    (loop [size 0]
      (if (.hasNext it)
        (do
          (.add arr-list (.next it))
          (recur (inc size)))
        [(.toArray arr-list) size]))))

(defn array-set! [arr idx v]
  (Array/set arr idx v))

(defn array-get [arr idx]
  (Array/get arr idx))

(defn array-copy! [src src-pos dest dest-pos length]
  (System/arraycopy src src-pos dest dest-pos length))
