(ns clojure.lang.operators
  (:refer-clojure :only [defmacro defn defn- nil? let cond and or complement])
  (:require [clojure.lang.platform.object :refer [equals type]]))

(defmacro when-not-nil [x y & body]
  `(let [x-nil?# (nil? ~x)
         y-nil?# (nil? ~y)]
     (cond
       (and x-nil?# y-nil?#)
       true
       (or x-nil?# y-nil?#)
       false
       :else
       ~@body)))

(defn values-equal? [x y]
  (when-not-nil
    x y
    (equals x y)))

(defn values-and-types-equal? [x y]
  (when-not-nil
    x y
    (and (equals (type x) (type y))
         (equals x y))))

(defmacro =
  "Loose eqaulity. Equality is determined by value."
  ([x] true)
  ([x y] `(values-equal? ~x ~y))
  ([x y & more] `(and (= ~x ~y) (= ~x ~@more))))

(defmacro ==
  "Strict eqaulity. Equality is determined by value and type."
  ([x] true)
  ([x y] `(values-and-types-equal? ~x ~y))
  ([x y & more] `(and (== ~x ~y) (== ~x ~@more))))

(defn not
  "Returns true if x is logical false, false otherwise."
  [x]
  (if x false true))

(defmacro not=
  "Same as (not (= obj1 obj2))."
  [& body]
  `(not (= ~@body)))

(defmacro not==
  "Same as (not (== obj1 obj2))."
  [& body]
  `(not (== ~@body)))
