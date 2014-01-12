(ns clojure.lang.operators
  (:refer-clojure :only [apply cond declare defmacro defn defn- if-let let nil? true?])
  (:require [clojure.lang.icomparable     :refer [-compare-to]]
            [clojure.lang.iequivalence    :refer [-equivalent?]]
            [clojure.lang.platform.object :refer [type]]
            [clojure.lang.platform.comparison]))

(defmacro and
  "Returns true if all expressions are logically truthy, false otherwise."
  ([] true)
  ([x] x)
  ([x & xs]
    `(let [and-expr# ~x]
       (if and-expr# (and ~@xs) and-expr#))))

(defmacro or
  "Returns true is any expression is logically truthy, false otherwise. If zero arguments are supplied then or will return nil."
  ([] nil)
  ([x] x)
  ([x & xs]
   `(if-let [or-expr# ~x]
      or-expr#
      (or ~@xs))))

(defn compare [x y]
  (cond
    (nil? x) -1
    (nil? y) 1
    :else
    (-compare-to x y)))

(defmacro when-not-nil [x y & body]
  {:private true}
  `(let [x-nil?# (nil? ~x)
         y-nil?# (nil? ~y)]
     (cond
       (and x-nil?# y-nil?#)
       true
       (or x-nil?# y-nil?#)
       false
       :else
       ~@body)))

(defn- values-equal? [x y]
  (when-not-nil
    x y
    (-equivalent? x y)))

(defn- values-and-types-equal? [x y]
  (when-not-nil
    x y
    (and (-equivalent? (type x) (type y))
         (-equivalent? x y))))

(defn =
  "Loose eqaulity. Equality is determined by value."
  ([x] true)
  ([x y] (values-equal? x y))
  ([x y & more] (and (= x y) (apply = x more))))

(defn ==
  "Strict eqaulity. Equality is determined by value and type."
  ([x] true)
  ([x y] (values-and-types-equal? x y))
  ([x y & more] (and (== x y) (apply == x more))))

(defn not
  "Returns true if x is logical false, false otherwise."
  [x]
  (if x false true))

(defn not=
  "Same as (not (= obj1 obj2))."
  [& args]
  (not (apply = args)))

(defn not==
  "Same as (not (== obj1 obj2))."
  [& args]
  (not (apply == args)))
