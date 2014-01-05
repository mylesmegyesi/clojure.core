(ns clojure.lang.comparison
  (:refer-clojure :only [defmacro defn defn- apply nil? let cond and or])
  (:require [clojure.lang.icomparable     :refer [-compare-to]]
            [clojure.lang.iequivalence    :refer [-equivalent?]]
            [clojure.lang.logical         :refer [not]]
            [clojure.lang.platform.comparison]
            [clojure.lang.platform.object :refer [type]]))

(defn compare [x y]
  (cond
    (nil? x) -1
    (nil? y) 1
    :else
    (-compare-to x y)))

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

(defn not=
  "Same as (not (= obj1 obj2))."
  [& args]
  (not (apply = args)))

(defn not==
  "Same as (not (== obj1 obj2))."
  [& args]
  (not (apply == args)))
