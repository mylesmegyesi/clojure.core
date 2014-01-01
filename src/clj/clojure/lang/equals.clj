(ns clojure.lang.equals
  (:refer-clojure :only [defmacro defn defn- nil? let cond and or complement])
  (:require [clojure.lang.equivalence     :refer [equivalent?]]
            [clojure.lang.ordered         :refer [compare-to]]
            [clojure.lang.platform.object :refer [type]]
            [clojure.lang.platform.equivalence]))

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
    (equivalent? x y)))

(defn values-and-types-equal? [x y]
  (when-not-nil
    x y
    (and (equivalent? (type x) (type y))
         (equivalent? x y))))

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

(defn compare [x y]
  (compare-to x y))
