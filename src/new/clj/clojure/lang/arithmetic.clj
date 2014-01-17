(ns clojure.lang.arithmetic
  (:refer-clojure :only [defn])
  (:require [clojure.lang.iarithmetic :refer [-add -subtract -multiply -divide -modulus -exponent]]
            [clojure.lang.platform.arithmetic]))

(defn + [x y] (-add x y))
(defn - [x y] (-subtract x y))
(defn * [x y] (-multiply x y))
(defn / [x y] (-divide x y))
(defn mod [x y] (-modulus x y))
(defn exp [x y] (-exponent x y))
