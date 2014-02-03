(ns clojure.lang.array
  (:refer-clojure :only [defn let loop nil? inc])
  (:require [clojure.lang.numbers :refer [unsafe-cast-int]]
            [clojure.next         :refer :all
             :exclude [inc] ; remove me
             ])
  (:import [java.lang.reflect Array]
           [java.util ArrayList]))

(defn make-array [type size]
  (Array/newInstance type (unsafe-cast-int size)))

(defn array-set! [arr idx v]
  (Array/set arr idx v))

(defn array-get [arr idx]
  (Array/get arr idx))

(defn array-copy! [src src-pos dest dest-pos length]
  (System/arraycopy src src-pos dest dest-pos length))

(defn array-length [arr]
  (Array/getLength arr))

(defn array-clone! [arr]
  (let [size (array-length arr)
        new-arr (make-array Object size)]
    (array-copy! arr 0 new-arr 0 size)
    new-arr))
