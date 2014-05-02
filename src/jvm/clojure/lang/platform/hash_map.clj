(ns clojure.lang.platform.hash-map
  (:refer-clojure :only [defmacro defn])
  (:require [clojure.lang.numbers :refer [unsafe-cast-int]])
  (:import [clojure.lang.platform.numbers Addition]
           [clojure.lang.platform.numbers Multiplication]))

(defmacro ->bitnum [n]
  `(unsafe-cast-int ~n))

(defn empty-object []
  (Object.))

(defmacro + [x y]
  `(. Addition (hashMapIntegerAdd ~x ~y)))

(defmacro * [x y]
  `(. Multiplication (hashMapIntegerMultiply ~x ~y)))
