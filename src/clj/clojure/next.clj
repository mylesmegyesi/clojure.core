(ns clojure.next ; eventually, this will be clojure.core
  (:refer-clojure :only [apply cond declare defmacro defn defn-
                         even? extend-type fn if-let let nil? number? require satisfies?
                         list list* loop format into < butlast last])
  (:require [clojure.lang.platform.equivalence]
            [clojure.lang.platform.exceptions :refer [new-argument-error]]
            [clojure.lang.protocols :refer :all]))

(require ['clojure.lang.platform.object :as 'platform-object])

(defn instance? [c x]
  (platform-object/instance? c x))

(defn identical? [x y]
  (platform-object/identical? x y))

(defn type [x]
  (platform-object/type x))

(require ['clojure.lang.numbers :refer ['-bit-count
                                        'bunsigned-shift-right
                                        'bshift-left
                                        'band
                                        'bor
                                        'bxor
                                        'add
                                        'increment
                                        'multiply
                                        'subtract
                                        'decrement
                                        'divide]])

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
    (do
    (-equivalent? x y))))

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

(defn unsigned-bit-shift-right [n shift]
  (bunsigned-shift-right n shift))

(defn bit-shift-left [n shift]
  (bshift-left n shift))

(defn bit-and
  ([n other] (band n other))
  ([n other & more] (clojure.core/reduce bit-and (bit-and n other) more)))

(defn bit-or
  ([n other] (bor n other))
  ([n other & more] (clojure.core/reduce bit-or (bit-or n other) more)))

(defn bit-xor
  ([n other] (bxor n other))
  ([n other & more] (clojure.core/reduce bit-xor (bit-xor n other) more)))

(defn bit-count [i]
  (-bit-count i))

(defn +
  ([] 0)
  ([x] (add x 0))
  ([x y] (add x y))
  ([x y & more] (clojure.core/reduce + (+ x y) more)))

(defn -
  ([x] (subtract x))
  ([x y] (subtract x y))
  ([x y & more] (clojure.core/reduce - (- x y) more)))

(defn *
  ([] 1)
  ([x] (multiply x 1))
  ([x y] (multiply x y))
  ([x y & more] (clojure.core/reduce * (* x y) more)))

(defn /
  ([x] (/ 1 x))
  ([x y] (divide x y))
  ([x y & more] (clojure.core/reduce / (/ x y) more)))

(defn inc [i]
  (increment i))

(defn dec [i]
  (decrement i))

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

(defn numerator [ratio]
  (-numerator ratio))

(defn denominator [ratio]
  (-denominator ratio))

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

(defn seq [s]
  (-seq s))

(defn sequential? [s]
  (satisfies? ISequential s))

(require ['clojure.lang.aseq])
(require ['clojure.lang.platform.seqable])

(declare atom)
(declare reset!)
(require ['clojure.lang.lazy-seq])

(defmacro lazy-seq [& s-body]
  (list 'clojure.lang.lazy-seq/make-lazy-seq (list* 'clojure.core/fn [] s-body)))

(require ['clojure.lang.cons :refer ['make-cons]])

(defn cons [elem seqable]
  (if (nil? seqable)
    (list elem)
    (make-cons elem (seq seqable))))

(extend-type nil
  ISeqable
  (-seq [this] nil))

(defn first [s]
  (-first (seq s)))

(defn next [s]
  (-next (seq s)))

(defn every? [pred s]
  (cond
    (nil? s) true
    (pred (first s)) (recur pred (next s))
    :else false))

(defn nth
  ([coll n]
    (-nth coll n))
  ([coll n not-found]
    (-nth coll n not-found)))

(defn empty? [seqable]
  (not (seq seqable)))

(require 'clojure.lang.platform.seqable)

(defn hash [obj]
  (-hash obj))

(defn key [entry]
  (-key entry))

(defn val [entry]
  (-val entry))

(defn assoc-seq [m kvs]
  (if kvs
    (let [n (next kvs)]
      (recur (-assoc m (first kvs) (first n)) (next n)))
    m))

(defn assoc
  ([m k v]
   (-assoc m k v))
  ([m k v & kvs]
   (assoc-seq (-assoc m k v) (seq kvs))))

(defn dissoc-seq [m ks]
  (if ks
    (recur (-dissoc m (first ks)) (next ks))
    m))

(defn dissoc
  ([m] m)
  ([m k] (-dissoc m k))
  ([m k & ks]
   (dissoc-seq (-dissoc m k) (seq ks))))

(defn contains-key? [m k]
  (-contains-key? m k))

(require ['clojure.lang.array :as 'arr])

(defn make-array
  ([size] (make-array Object size))
  ([type size]
   (arr/make-array type size)))

(defn aset [arr i val]
  (arr/array-set! arr i val))

(defn aget [arr i]
  (arr/array-get arr i))

(defn alength [arr]
  (arr/array-length arr))

(defn aclone [arr]
  (arr/array-clone arr))

(defn acopy [src src-pos dest dest-pos length]
  (arr/array-copy src src-pos dest dest-pos length))

(defn into-array
  ([seqable] (into-array Object seqable))
  ([type seqable]
   (let [s (seq seqable)
         size (count s)
         arr (make-array type size)]
     (loop [i 0 s s]
       (if (nil? s)
         arr
         (do
           (aset arr i (first s))
           (recur (clojure.core/inc i) (next s))))))))

(require ['clojure.lang.persistent-array-map :refer ['new-array-map]])

(defn array-map [& args]
  (let [sargs (seq args)
        size (count sargs)]
    (if (even? size)
      (new-array-map (into-array sargs) size (/ size 2) nil)
      (throw (new-argument-error
               (format "PersistentArrayMap can only be created with even number of arguments: %s arguments given"
                       size))))))

(require ['clojure.lang.persistent-hash-map :refer ['new-hash-map 'EMPTY-HASH-MAP]])

(defn hash-map [& kvs]
  (let [kvs-seq (seq kvs)]
    (if kvs-seq
      (let [size (count kvs-seq)]
        (if (even? size)
          (loop [s kvs-seq m EMPTY-HASH-MAP]
            (if s
              (recur (next (next s)) (assoc m (first s) (first (next s))))
              m))
          (throw (new-argument-error
                   (format "PersistentHashMap can only be created with even number of arguments: %s arguments given"
                           size)))))
      EMPTY-HASH-MAP)))

(defn get-validator [this]
  (-get-validator this))

(defn set-validator! [this validator-fn]
  (-set-validator! this validator-fn))

(defn add-watch [this k f]
  (-add-watch this k f))

(defn remove-watch [this k]
  (-remove-watch this k))

(defn compare-and-set! [atm old-val new-val]
  (-compare-and-set! atm old-val new-val))

(defn reset! [atm new-val]
  (-reset! atm new-val))

(defn swap!
  ([atm f] (-swap! atm f []))
  ([atm f x] (-swap! atm f [x]))
  ([atm f x y] (-swap! atm f [x y]))
  ([atm f x y & args] (-swap! atm f (into [x y] args))))

(require ['clojure.lang.atomic-ref :refer ['new-atomic-ref]])
(require ['clojure.lang.atom :refer ['new-atom]])

(defn atom
  ([state]
    (atom state :meta nil :validator nil))
  ([state & args]
    (let [config (apply array-map args)]
      (new-atom (new-atomic-ref state)
                (get config :meta)
                (get config :validator)
                {}))))

(defn memoize [f]
  (let [cache-atom (atom (hash-map))]
    (fn [& args]
      (let [cache (deref cache-atom)]
        (if (contains? cache args)
          (get cache args)
          (let [return-value (apply f args)]
            (swap! cache-atom assoc args return-value)
            return-value))))))

(defn comparator [predicate]
  (fn [x y]
    (if (predicate x y) -1 0)))

(defn- compare-numbers [x y]
  (cond
    (< x y) -1
    (< y x) 1
    :else 0))

(defn compare [x y]
  (if (= x y)
    0
    (if (not (nil? x))
      (if (nil? y)
        1
        (if (number? x)
          (compare-numbers x y)
          (-compare-to x y)))
      -1)))

(require ['clojure.lang.hash :refer ['hash-combine]])
(require ['clojure.lang.platform.show :refer ['build-string]])
(require ['clojure.lang.symbol :as 'sym])

(defn str
  ([] "")
  ([x]
   (if (nil? x) "" (-show x)))
  ([x & more]
   (build-string (clojure.core/cons x more))))

(defn symbol? [x]
  (sym/symbol? x))

(defn symbol
  ([name]
   (if (symbol? name)
     name
     (let [parts (clojure.string/split name #"/")]
       (if (= 1 (clojure.core/count parts))
         (symbol nil (clojure.core/first parts))
         (symbol (clojure.string/join "/" (butlast parts)) (last parts))))))
  ([ns name]
   (if (nil? name)
     (throw (Exception. "Can't create symbol with nil name")))
   (sym/new-symbol ns name (if ns (str ns "/" name) name)
               (hash (hash-combine (hash name) (hash ns))) {})))

(require ['clojure.lang.keyword :as 'kwd])

(defn keyword? [x]
  (kwd/keyword? x))

(defn keyword
  ([n]
   (let [sym (symbol n)]
     (keyword (namespace sym) (name sym))))
  ([ns name]
   (let [sym (symbol ns name)
         hash-code (hash (clojure.core/+ (hash sym) 0x9e3779b9))]
     (kwd/new-keyword ns name (str ":" sym) hash-code {} sym))))

(defn reduce
  ([f coll]
   (if-let [s (seq coll)]
     (reduce f (first s) (next s))
     (f)))
  ([f v coll]
    (loop [s coll
           acc v]
      (if (nil? s)
        acc
        (let [next-s (seq s)
              next-acc (f acc (first next-s))]
          (recur (next next-s) next-acc))))))
