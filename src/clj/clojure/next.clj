(ns clojure.next ; eventually, this will be clojure.core
  (:refer-clojure :only [apply cond declare defmacro defn defn-
                         extend-type fn if-let let nil? require satisfies?])
  (:require [clojure.lang.numbers  :refer [-bit-unsigned-shift-right
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
            [clojure.lang.platform.equivalence]
            [clojure.lang.protocols :refer :all]))

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

(defn count [obj]
  (-count obj))

(extend-type nil
  ICounted
  (-count [this] 0))

(defn deref [obj]
  (-deref obj))

(defn contains? [coll k]
  (-includes? coll k))

(defn get
  ([coll k] (-lookup coll k nil))
  ([coll k not-found] (-lookup coll k not-found)))

(defn alter-meta! [this f & args]
  (-alter-meta! this f args))

(defn meta [this]
  (-meta this))

(defn reset-meta! [this new-meta]
  (-reset-meta! this new-meta))

(defn with-meta [this new-meta]
  (-with-meta this new-meta))

(defn name [named]
  (-name named))

(defn namespace [named]
  (-namespace named))

(defn seq [i]
  (-seq i))

(extend-type nil
  ISeqable
  (-seq [this] nil))

(defn first [s]
  (-first s))

(defn next [s]
  (-next s))

(defn every? [pred seqable]
  (let [s (seq seqable)]
    (cond
      (nil? s) true
      (pred (first s)) (recur pred (next s))
      :else false)))

(defn empty? [seqable]
  (not (seq seqable)))

(defn sequential? [s]
  (satisfies? ISequential s))

(require 'clojure.lang.platform.seqable)

(defn get-validator [this]
  (-get-validator this))

(defn set-validator! [this validator-fn]
  (-set-validator! this validator-fn))

(defn add-watch [this k f]
  (-add-watch this k f))

(defn remove-watch [this k]
  (-remove-watch this k))
