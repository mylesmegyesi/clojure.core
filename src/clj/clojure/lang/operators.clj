(ns clojure.lang.operators
  (:refer-clojure :only [apply cond declare defmacro defn defn- if-let let nil? true?])
  (:require [clojure.lang.platform.equivalence]
            [clojure.lang.platform.numbers  :refer [-bit-unsigned-shift-right
                                                    -bit-shift-left
                                                    -bit-and
                                                    -bit-count
                                                    -bit-or
                                                    -bit-xor
                                                    -add
                                                    -subtract
                                                    -multiply
                                                    -increment
                                                    -decrement]]
            [clojure.lang.protocols         :refer [-equivalent? -equal?]]))

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

(defn- equal? [x y]
  (when-not-nil
    x y
    (-equal? x y)))

(defn- equivalent? [x y]
  (when-not-nil
    x y
    (-equivalent? x y)))

(defn =
  "Eqaulity. Calls the -equal? method on the first argument."
  ([x] true)
  ([x y] (equal? x y))
  ([x y & more] (and (= x y) (apply = y more))))

(defn ==
  "Equivalence. Calls the -equivalent? method on the first argument."
  ([x] true)
  ([x y] (equivalent? x y))
  ([x y & more] (and (== x y) (apply == y more))))

(defn not
  "Returns true if x is logical false, false otherwise."
  [x]
  (if x false true))

(defn boolean
  "Returns true if x is logical true, false otherwise."
  [x]
  (if x true false))

(defn not=
  "Same as (not (= obj1 obj2))."
  [& args]
  (not (apply = args)))

(defn not==
  "Same as (not (== obj1 obj2))."
  [& args]
  (not (apply == args)))

(defn bit-unsigned-shift-right [n shift]
  (-bit-unsigned-shift-right n shift))

(defn bit-shift-left [n shift]
  (-bit-shift-left n shift))

(defn bit-and [n other]
  (-bit-and n other))

(defn bit-or [n other]
  (-bit-or n other))

(defn bit-xor [n other]
  (-bit-xor n other))

(defn bit-count [i]
  (-bit-count i))

(defn + [x y]
  (-add x y))

(defn - [x y]
  (-subtract x y))

(defn * [x y]
  (-multiply x y))

(defn inc [i]
  (-increment i))

(defn dec [i]
  (-decrement i))
