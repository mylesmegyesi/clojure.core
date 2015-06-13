(ns clojure.lang.array
  (:refer-clojure :only [defmacro let loop inc])
  (:require [clojure.lang.numbers :refer [unsafe-cast-int]]
            [clojure.next         :refer :all :exclude [inc]])
  (:import [java.lang.reflect Array]))

(defmacro make-array [type size]
  `(Array/newInstance ~type (unsafe-cast-int ~size)))

(defmacro array-set! [arr idx v]
  `(Array/set ~arr ~idx ~v))

(defmacro array-get [arr idx]
  `(Array/get ~arr ~idx))

(defmacro array-copy [src src-pos dest dest-pos length]
  `(System/arraycopy ~src ~src-pos ~dest ~dest-pos ~length))

(defmacro array-length [arr]
  `(Array/getLength ~arr))

(defmacro array-clone [arr]
  `(let [arr# ~arr
         size# (array-length arr#)
         new-arr# (make-array Object size#)]
     (array-copy arr# 0 new-arr# 0 size#)
     new-arr#))
