(ns clojure.lang.platform.hash-map
  (:refer-clojure :only [defmacro defn int])
  (:require [clojure.lang.numbers :refer [unsafe-cast-int]])
  (:import [clojure.lang.platform.numbers Addition]
           [clojure.lang.platform.numbers Multiplication]
           [clojure.lang.platform.numbers Subtraction]))

(defmacro ->bitnum [n]
  `(unsafe-cast-int ~n))

(defn empty-object []
  (Object.))

(def integer-one (int 1))

(defmacro + [x y]
  `(. Addition (integerPreserveAdd ~x ~y)))

(defmacro inc [x]
  `(. Addition (integerPreserveAdd ~x integer-one)))

(defmacro * [x y]
  `(. Multiplication (integerPreserveMultiply ~x ~y)))

(defmacro - [x y]
  `(. Subtraction (integerPreserveSubtract ~x ~y)))

(defmacro dec [x]
  `(. Subtraction (integerPreserveSubtract ~x integer-one)))

