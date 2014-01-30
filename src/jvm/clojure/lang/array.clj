(ns clojure.lang.array
  (:refer-clojure :only [defn let loop nil? inc])
  (:require [clojure.lang.counted          :refer [count]]
            ;[clojure.lang.operators        :refer [inc]]
            [clojure.lang.seqable          :refer [seq]]
            [clojure.lang.seq              :refer [first next]]
            [clojure.lang.platform.numbers :refer [unsafe-cast-int]])
  (:import [java.lang.reflect Array]
           [java.util ArrayList]))

(defn make-array
  ([size] (make-array Object size))
  ([type size]
   (Array/newInstance type (unsafe-cast-int size))))

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
        new-arr (make-array size)]
    (array-copy! arr 0 new-arr 0 size)
    new-arr))

(defn into-array
  ([seqable] (into-array Object seqable))
  ([type seqable]
   (let [s (seq seqable)
         size (count s)
         arr (make-array type size)]
     (loop [i 0 s s]
       (if (nil? s)
         arr
         (do
           (array-set! arr i (first s))
           (recur (inc i) (next s))))))))
