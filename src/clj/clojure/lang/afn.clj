(ns clojure.lang.afn
  (:refer-clojure :only [apply concat cond case defn defn- defmacro fn gensym let list loop range reverse some vec ->>])
  (:require [clojure.next :refer :all]
            [clojure.lang
              [callable          :as    call]
              [deftype           :refer [deftype]]
              [exceptions        :refer :all]
              [object            :refer [class-name]]
              [runnable          :as    run]
              [protocols         :refer [IFn -invoke -apply-to]]]))

(defn throw-arity [i c]
  (let [n (class-name c)]
    (throw (new-arity-exception i n))))

(defn- find-relevant-methods [body]
  (->>
    (clojure.core/reduce
      (fn [[stripped-body relevant-methods] line]
        (cond
          (or (= 'IFn line) (= 'clojure.lang.protocols.IFn line))
             [stripped-body relevant-methods]
          (and (clojure.core/seq? line) (= '-invoke (first line)))
             [stripped-body (clojure.core/conj relevant-methods line)]
          :else
             [(clojure.core/conj stripped-body line) relevant-methods]))
      [(list) (list)] body)
    (clojure.core/map reverse)
    vec))

(defn- contains-arity? [relevant-methods arity]
  (some #(= (count (next (first (next %)))) arity) relevant-methods))

(defn- add-missing-arities [relevant-methods]
  (concat
    relevant-methods
    (clojure.core/reduce
      (fn [new-methods i]
        (if (contains-arity? relevant-methods i)
          new-methods
          (let [arg-range (range 1 (inc i))
                args (vec (clojure.core/cons 'this (clojure.core/map #(clojure.core/symbol (str "arg" %)) arg-range)))]
            (clojure.core/conj new-methods (list '-invoke args (list 'clojure.lang.afn/throw-arity i 'this))))))
      (list) (range 1 20))))

(defn apply-to [ifn args]
  (case (count args)
    0 (-invoke ifn)
    1 (-invoke ifn (nth args 0))
    2 (-invoke ifn (nth args 0) (nth args 1))
    3 (-invoke ifn (nth args 0) (nth args 1) (nth args 2))
    4 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3))
    5 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4))
    6 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5))
    7 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6))
    8 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7))
    9 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8))
    10 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8) (nth args 9))
    11 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8) (nth args 9) (nth args 10))
    12 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8) (nth args 9) (nth args 10) (nth args 11))
    13 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8) (nth args 9) (nth args 10) (nth args 11) (nth args 12))
    14 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8) (nth args 9) (nth args 10) (nth args 11) (nth args 12) (nth args 13))
    15 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8) (nth args 9) (nth args 10) (nth args 11) (nth args 12) (nth args 13) (nth args 14))
    16 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8) (nth args 9) (nth args 10) (nth args 11) (nth args 12) (nth args 13) (nth args 14) (nth args 15))
    17 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8) (nth args 9) (nth args 10) (nth args 11) (nth args 12) (nth args 13) (nth args 14) (nth args 15) (nth args 16))
    18 (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8) (nth args 9) (nth args 10) (nth args 11) (nth args 12) (nth args 13) (nth args 14) (nth args 15) (nth args 16) (nth args 17))
    (let [remainder-args (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest args))))))))))))))))))
          empty-transient (transient (vector))
          remainder-args-vec (if remainder-args
                               (loop [xs remainder-args v empty-transient]
                                 (if xs
                                   (recur (next xs) (conj! v (first xs)))
                                   (persistent! v)))
                               (persistent! empty-transient))]
          (-invoke ifn (nth args 0) (nth args 1) (nth args 2) (nth args 3) (nth args 4) (nth args 5) (nth args 6) (nth args 7) (nth args 8) (nth args 9) (nth args 10) (nth args 11) (nth args 12) (nth args 13) (nth args 14) (nth args 15) (nth args 16) (nth args 17) remainder-args-vec))))

(defmacro deffn [t bindings & body]
  (if (some #(or (= 'IFn %) (= 'clojure.lang.protocols.IFn %)) body)
    (let [[stripped-body relevant-methods] (find-relevant-methods body)
          ifn-methods (add-missing-arities relevant-methods)]
      `(let [this-sym# (gensym "this")
             args-sym# (gensym "args")]
        (deftype ~t ~bindings
          ~@stripped-body
          run/base-runnable
          (run/runnable-method [this-sym#]
            (-invoke this-sym#)
            nil)
          call/base-callable
          (call/callable-method [this-sym#]
            (-invoke this-sym#))
          IFn
          (-apply-to [this-sym# args-sym#] (apply-to this-sym# args-sym#))
          ~@ifn-methods)))
    (throw (new-argument-error "Tried to invoke deffn without an IFn implementation"))))

