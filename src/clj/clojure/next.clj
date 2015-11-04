(ns clojure.next ; eventually, this will be clojure.core
  (:refer-clojure :only [*assert*
                         apply binding cond declare defmacro defmulti defmethod defn defn-
                         extend-type fn if-let let neg? pos? require satisfies? range
                         doseq for list list* load loop format pr-str into butlast when when-let])
  (:require [clojure.lang.equivalence]
            [clojure.lang.object     :as    platform-object]
            [clojure.lang.exceptions :refer [new-assertion-error new-argument-error new-exception new-out-of-bounds-exception]]
            [clojure.lang.random     :refer [rand-float]]
            [clojure.lang.protocols  :refer :all]))

(def ^:dynamic *clojure-version*
  {:major       1
   :minor       6
   :incremental 0})

(declare cons str seq first next reduce not)

(defn clojure-version []
  (str (:major *clojure-version*) "."
       (:minor *clojure-version*) "."
       (:incremental *clojure-version*)))

(defmacro comment [& body])

(defn instance? [c x]
  (platform-object/instance? c x))

(defn identical? [x y]
  (platform-object/identical? x y))

(defn counted? [c]
  (satisfies? ICounted c))

(defn class [x]
  (platform-object/class x))

(defn class? [c]
  (instance? platform-object/base-class c))

(defn nil? [n]
  (identical? n nil))

(defn true? [t]
  (identical? t true))

(defn false? [f]
  (identical? f false))

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

(defn ratio? [x]
  (satisfies? IRatio x))

(defn integer? [x]
  (satisfies? IInteger x))

(defn float? [x]
  (satisfies? IFloat x))

(defn decimal? [x]
  (satisfies? IDecimal x))

(defn rational? [x]
  (or (integer? x) (ratio? x) (decimal? x)))

(defn associative? [x]
  (satisfies? IAssociative x))

(defn seq? [s]
  (satisfies? ISeq s))

(defn sequential? [s]
  (satisfies? ISequential s))

(defn vector? [v]
  (satisfies? IPersistentVector v))

(defn map? [m]
  (satisfies? IPersistentMap m))

(defn set? [s]
  (satisfies? IPersistentSet s))

(defn coll? [c]
  (satisfies? IPersistentCollection c))

(defn list? [l]
  (satisfies? IPersistentList l))

(defn string? [s]
  (instance? platform-object/platform-string s))

(defn first [s]
  (-first (seq s)))

(defn ffirst [s]
  (first (first s)))

(defn next [s]
  (-next (seq s)))

(defn nfirst [s]
  (next (first s)))

(defn nnext [s]
  (next (next s)))

(defn fnext [s]
  (first (next s)))

(defn rest [s]
  (-more (seq s)))

(defn last [s]
  (if (next s)
    (recur (next s))
    (first s)))

(defn second [s]
  (first (next s)))

(defn empty [coll]
  (-empty coll))

(defn empty? [seqable]
  (not (seq seqable)))

(defn hash [obj]
  (-hash obj))

(declare bigint)
(require '[clojure.lang.numbers :as platform-numbers])

(defn number? [x]
  (platform-numbers/number? x))

(defmacro when-not-nil [x y & body]
  ^:private
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
  (when-not-nil x y
    (if (and (number? x) (number? y))
      (platform-numbers/equal? x y)
      (-equal? x y))))

(defn =
  "Equality. When provided with numbers performs numbers-equal?. Else, calls the -equal? method on the first argument."
  ([x] true)
  ([x y] (equal? x y))
  ([x y & more] (and (= x y) (apply = y more))))

(defn ==
  "Equivalence. Calls the platform numbers-equivalent? function with the arguments"
  ([x] true)
  ([x y] (platform-numbers/equivalent? x y))
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

(defn byte [x]
  (platform-numbers/->byte x))

(defn unchecked-byte [x]
  (platform-numbers/unchecked->byte x))

(defn short [x]
  (platform-numbers/->short x))

(defn unchecked-short [x]
  (platform-numbers/unchecked->short x))

(defn int [x]
  (platform-numbers/->int x))

(defn unchecked-int [x]
  (platform-numbers/unchecked->int x))

(defn long [x]
  (platform-numbers/->long x))

(defn unchecked-long [x]
  (platform-numbers/unchecked->long x))

(defn float [x]
  (platform-numbers/->float x))

(defn unchecked-float [x]
  (platform-numbers/unchecked->float x))

(defn double [x]
  (platform-numbers/->double x))

(defn unchecked-double [x]
  (platform-numbers/unchecked->double x))

(defn bigint [x]
  (platform-numbers/->bigint x))

(defn biginteger [x]
  (platform-numbers/->biginteger x))

(defn bigdec [x]
  (platform-numbers/->bigdec x))

(defn bit-shift-right [n shift]
  (platform-numbers/bit-shift-right n shift))

(defn unsigned-bit-shift-right [n shift]
  (platform-numbers/unsigned-bit-shift-right n shift))

(defn bit-shift-left [n shift]
  (platform-numbers/bit-shift-left n shift))

(defn bit-not [x] (platform-numbers/bit-not x))

(defn bit-and
  ([n other] (platform-numbers/bit-and n other))
  ([n other & more] (reduce bit-and (bit-and n other) more)))

(defn bit-and-not
  ([n other] (platform-numbers/bit-and-not n other))
  ([n other & more] (reduce bit-and-not (bit-and-not n other) more)))

(defn bit-or
  ([n other] (platform-numbers/bit-or n other))
  ([n other & more] (reduce bit-or (bit-or n other) more)))

(defn bit-xor
  ([n other] (platform-numbers/bit-xor n other))
  ([n other & more] (reduce bit-xor (bit-xor n other) more)))

(defn bit-clear [x location]
  (platform-numbers/bit-clear x location))

(defn bit-set [x location]
  (platform-numbers/bit-set x location))

(defn bit-flip [x location]
  (platform-numbers/bit-flip x location))

(defn bit-test [x location]
  (platform-numbers/bit-test x location))

(defn +
  ([] 0)
  ([x] (platform-numbers/+ x 0))
  ([x y] (platform-numbers/+ x y))
  ([x y & more] (reduce + (+ x y) more)))

(defn +'
  ([] 0)
  ([x] (platform-numbers/+' x 0))
  ([x y] (platform-numbers/+' x y))
  ([x y & more] (reduce +' (+' x y) more)))

(defn -
  ([x] (platform-numbers/- x))
  ([x y] (platform-numbers/- x y))
  ([x y & more] (reduce - (- x y) more)))

(defn -'
  ([x] (platform-numbers/-' x))
  ([x y] (platform-numbers/-' x y))
  ([x y & more] (reduce -' (-' x y) more)))

(defn unchecked-subtract [x y]
  (platform-numbers/unchecked-subtract x y))

(defn unchecked-subtract-int [x y]
  (platform-numbers/unchecked-subtract-int x y))

(defn unchecked-negate [x]
  (platform-numbers/unchecked-negate x))

(defn unchecked-negate-int [x]
  (platform-numbers/unchecked-negate-int x))

(defn *
  ([] 1)
  ([x] (platform-numbers/* x 1))
  ([x y] (platform-numbers/* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn *'
  ([] 1)
  ([x] (platform-numbers/*' x 1))
  ([x y] (platform-numbers/*' x y))
  ([x y & more] (reduce *' (*' x y) more)))

(defn unchecked-multiply [x y]
  (platform-numbers/unchecked-multiply x y))

(defn unchecked-multiply-int [x y]
  (platform-numbers/unchecked-multiply-int x y))

(defn /
  ([x] (/ 1 x))
  ([x y] (platform-numbers// x y))
  ([x y & more] (reduce / (/ x y) more)))

(defn zero? [i]
  (platform-numbers/zero? i))

(defn even? [i]
  (if (integer? i)
    (zero? (bit-and (platform-numbers/unchecked->long i) 1))
    (throw (new-argument-error (str "Argument must be an integer: " i)))))

(defn odd? [i]
  (not (even? i)))

(defn quot [n div]
  (platform-numbers/quot n div))

(defn rem [n div]
  (platform-numbers/rem n div))

(defn mod [n div]
  (let [modulus (rem n div)]
    (if (or (zero? modulus) (= (pos? n) (pos? div)))
      modulus
      (+ modulus div))))

(defn inc [i]
  (platform-numbers/inc i))

(defn unchecked-inc [i]
  (platform-numbers/unchecked-inc i))

(defn unchecked-inc-int [i]
  (platform-numbers/unchecked-inc-int i))

(defn inc' [i]
  (platform-numbers/inc' i))

(defn dec [i]
  (platform-numbers/dec i))

(defn unchecked-dec [i]
  (platform-numbers/unchecked-dec i))

(defn dec' [i]
  (platform-numbers/dec' i))

(defn max
  ([x] x)
  ([x y] (platform-numbers/max x y))
  ([x y & more]
    (reduce max (max x y) more)))

(defn min
  ([x] x)
  ([x y] (platform-numbers/min x y))
  ([x y & more]
    (reduce min (min x y) more)))

(defn rationalize [n]
  (platform-numbers/rationalize n))

(defn <
  ([a] true)
  ([a b] (platform-numbers/< a b))
  ([a b & more]
    (if (< a b)
      (if (next more)
        (recur b (first more) (next more))
        (< b (first more)))
      false)))

(defn <=
  ([a] true)
  ([a b] (platform-numbers/<= a b))
  ([a b & more]
    (if (<= a b)
      (if (next more)
        (recur b (first more) (next more))
        (<= b (first more)))
      false)))

(defn >
  ([a] true)
  ([a b] (platform-numbers/< b a))
  ([a b & more]
    (if (> a b)
      (if (next more)
        (recur b (first more) (next more))
        (> b (first more)))
      false)))

(defn >=
  ([a] true)
  ([a b] (platform-numbers/<= b a))
  ([a b & more]
    (if (>= a b)
      (if (next more)
        (recur b (first more) (next more))
        (>= b (first more)))
      false)))

(require ['clojure.lang.size :refer ['platform-count]])

(defn count [obj]
  (cond
    (counted? obj)
      (-count obj)
    (nil? obj)
      0
    (satisfies? IPersistentCollection obj)
      (loop [s (seq obj)
             cnt 0]
        (if s
          (if (counted? s)
            (recur (next s) (+ cnt (count s)))
            (recur (next s) (inc cnt)))
          cnt))
    :else
      (platform-count obj)))

(defn- nth-sequential
  ([coll n]
    (loop [s (seq coll)
           cnt 0]
      (if (nil? s)
        (throw (new-out-of-bounds-exception ""))
        (if (= cnt n)
          (first s)
          (recur (next s) (inc cnt))))))
  ([coll n not-found]
    (loop [s (seq coll)
           cnt 0]
      (if (nil? s)
        not-found
        (if (= cnt n)
          (first s)
          (recur (next s) (inc cnt)))))))

(defn nth
  ([coll n]
    (cond
      (satisfies? IIndexed coll) (-nth coll n)
      (satisfies? ISequential coll) (nth-sequential coll n)))
  ([coll n not-found]
    (cond
      (satisfies? IIndexed coll) (-nth coll n not-found)
      (satisfies? ISequential coll) (nth-sequential coll n not-found))))

(require '[clojure.lang.primitive-array :refer :all])

(defn booleans [arr]
  (to-booleans arr))

(defn bytes [arr]
  (to-bytes arr))

(defn chars [arr]
  (to-chars arr))

(defn shorts [arr]
  (to-shorts arr))

(defn floats [arr]
  (to-floats arr))

(defn doubles [arr]
  (to-doubles arr))

(defn ints [arr]
  (to-ints arr))

(defn longs [arr]
  (to-longs arr))

(require ['clojure.lang.array :as 'arr])

(defn aget [arr i]
  (arr/array-get arr i))

(defn aset [arr i val]
  (arr/array-set! arr i val))

(defn aset-byte
  ([arr i v]
    (arr/array-set-byte! arr i (byte v)))
  ([arr i i2 & vs]
    (apply aset-byte (aget arr i) i2 vs)))

(defn aset-short
  ([arr i v]
    (arr/array-set-short! arr i (short v)))
  ([arr i i2 & vs]
    (apply aset-short (aget arr i) i2 vs)))

(defn aset-int
  ([arr i v]
    (arr/array-set-int! arr i (int v)))
  ([arr i i2 & vs]
    (apply aset-int (aget arr i) i2 vs)))

(defn aset-long
  ([arr i v]
    (arr/array-set-long! arr i (long v)))
  ([arr i i2 & vs]
    (apply aset-long (aget arr i) i2 vs)))

(defn aset-float
  ([arr i v]
    (arr/array-set-float! arr i (float v)))
  ([arr i i2 & vs]
    (apply aset-float (aget arr i) i2 vs)))

(defn aset-double
  ([arr i v]
    (arr/array-set-double! arr i (double v)))
  ([arr i i2 & vs]
    (apply aset-double (aget arr i) i2 vs)))

(defn alength [arr]
  (arr/array-length arr))

(defn aclone [arr]
  (arr/array-clone arr))

(defn acopy [src src-pos dest dest-pos length]
  (arr/array-copy src src-pos dest dest-pos length))

(defn make-array
  ([t len]
    (arr/make-array t (int len)))
  ([t len & lens]
    (arr/make-array t len lens)))

(defn into-array
  ([seqable] (into-array platform-object/base-object seqable))
  ([type seqable]
   (let [s (seq seqable)
         size (count s)
         arr (make-array type size)]
     (loop [i 0 s s]
       (if (nil? s)
         arr
         (do
           (aset arr i (first s))
           (recur (inc i) (next s))))))))

(defn to-array [coll]
  (if (nil? coll)
    arr/EMPTY-ARRAY
    (arr/to-array coll)))

(defn object-array [seq-or-size]
  (if (number? seq-or-size)
    (make-array platform-object/base-object seq-or-size)
    (into-array platform-object/base-object seq-or-size)))

(defn- primitive-array
  ([seq-or-size arr-fn]
    (if (number? seq-or-size)
      (arr-fn seq-or-size)
      (loop [i 0
             arr (arr-fn (count seq-or-size))
             sq (seq seq-or-size)]
        (if sq
          (do
            (aset arr i (first sq))
            (recur (inc i) arr (next sq)))
          arr))))
  ([size init-val-or-seq t arr-fn]
    (let [arr (arr-fn size)]
      (if (instance? t init-val-or-seq)
        (do
          (doseq [i (range 0 size)]
            (aset arr i init-val-or-seq))
          arr)
        (loop [i 0
               sq (seq init-val-or-seq)]
          (if (or (>= i size) (nil? sq))
            arr
            (do
              (aset arr i (first sq))
              (recur (inc i) (next sq)))))))))

(defmacro amap [a idx ret expr]
  `(let [a# ~a
         ~ret (aclone a#)]
     (loop [~idx 0]
       (if (< ~idx (alength a#))
         (do
           (aset ~ret ~idx ~expr)
           (recur (unchecked-inc ~idx)))
         ~ret))))

(defmacro areduce [a idx ret init expr]
 `(let [a# ~a]
     (loop [~idx 0 ~ret ~init]
       (if (< ~idx  (alength a#))
         (recur (unchecked-inc ~idx) ~expr)
         ~ret))))

(defn byte-array
  ([seq-or-size]
    (primitive-array seq-or-size byte-array-for-size))
  ([size init-val-or-seq]
    (primitive-array size init-val-or-seq platform-numbers/platform-byte byte-array-for-size)))

(defn short-array
  ([seq-or-size]
    (primitive-array seq-or-size short-array-for-size))
  ([size init-val-or-seq]
    (primitive-array size init-val-or-seq platform-numbers/platform-short short-array-for-size)))

(defn int-array
  ([seq-or-size]
    (primitive-array seq-or-size int-array-for-size))
  ([size init-val-or-seq]
    (primitive-array size init-val-or-seq platform-numbers/platform-int int-array-for-size)))

(defn long-array
  ([seq-or-size]
    (primitive-array seq-or-size long-array-for-size))
  ([size init-val-or-seq]
    (primitive-array size init-val-or-seq platform-numbers/platform-long long-array-for-size)))

(defn float-array
  ([seq-or-size]
    (primitive-array seq-or-size float-array-for-size))
  ([size init-val-or-seq]
    (primitive-array size init-val-or-seq platform-numbers/platform-float float-array-for-size)))

(defn double-array
  ([seq-or-size]
    (primitive-array seq-or-size double-array-for-size))
  ([size init-val-or-seq]
    (primitive-array size init-val-or-seq platform-numbers/platform-double double-array-for-size)))

(defn boolean-array
  ([seq-or-size]
    (primitive-array seq-or-size boolean-array-for-size))
  ([size init-val-or-seq]
    (primitive-array size init-val-or-seq platform-boolean boolean-array-for-size)))

(defn char-array
  ([seq-or-size]
    (primitive-array seq-or-size char-array-for-size))
  ([size init-val-or-seq]
    (primitive-array size init-val-or-seq platform-char char-array-for-size)))

(defn alter-meta! [m f & args]
  (-alter-meta! m f args))

(defn meta [m]
  (when (satisfies? IMeta m)
    (-meta m)))

(defn reset-meta! [m new-meta]
  (-reset-meta! m new-meta))

(defn with-meta [m new-meta]
  (-with-meta m new-meta))

(defn vary-meta [m f & args]
  (with-meta m (apply f (meta m) args)))

(defn identity [x] x)

(require ['clojure.lang.sequence :refer ['platform-seq 'make-iterator-seq]])

(defn seq [s]
  (cond
    (satisfies? ISeqable s)
      (-seq s)
    (nil? s)
      nil
    :else
      (platform-seq s)))

(defn iterator-seq [iter]
  (make-iterator-seq iter))

(require ['clojure.lang.delay :refer ['new-delay '-force 'is-delay?]])

(defmacro delay [& body]
  (list 'clojure.lang.delay/new-delay (list* 'clojure.core/fn [] body)))

(defn delay? [d]
  (is-delay? d))

(defn deref
  ([obj] (-deref obj))
  ([obj timeout-ms timeout-val]
    (-blocking-deref obj timeout-ms timeout-val)))

(defn realized? [obj]
  (-is-realized? obj))

(defn force [obj]
  (-force obj))

(defn get
  ([coll k] (get coll k nil))
  ([coll k not-found]
    (if (satisfies? ILookup coll)
      (-lookup coll k not-found))))

(defn numerator [ratio]
  (-numerator ratio))

(defn denominator [ratio]
  (-denominator ratio))

(defn type [x]
  (or (get (meta x) :type) (class x)))

(defn contains? [coll k]
  (cond
    (nil? coll) false
    (satisfies? IAssociative coll) (-contains-key? coll k)
    (map? coll) (-contains-key? coll k)
    (set? coll) (-contains? coll k)
    :else (throw (new-argument-error (str "contains? not supported on type: " (type coll))))))

(defn name [named]
  (-name named))

(defn namespace [named]
  (-namespace named))

(defn peek [coll]
  (when coll
    (-peek coll)))

(defn pop [coll]
  (when coll
    (-pop coll)))

(defn rand
  ([] (rand-float))
  ([n] (* n (rand))))

(defn rand-int [n]
  (int (rand n)))

(require ['clojure.lang.aseq])
(require ['clojure.lang.seqable])

(require ['clojure.lang.cons :refer ['make-cons]])

(defn cons [elem seqable]
  (if (nil? seqable)
    (list elem)
    (if (satisfies? ISeq seqable)
      (make-cons elem seqable)
      (make-cons elem (seq seqable)))))

(defn chunk-first [s]
  (-chunked-first s))

(defn chunk-next [s]
  (-chunked-next s))

(defn chunk-rest [s]
  (-chunked-more s))

(require '[clojure.lang.chunk-buffer :refer :all])

(defn ^:static ^clojure.lang.chunk_buffer.ChunkBuffer chunk-buffer ^clojure.lang.chunk_buffer.ChunkBuffer [capacity]
  (make-chunk-buffer capacity))

(defn ^:static chunk-append [^clojure.lang.chunk_buffer.ChunkBuffer b o]
  (-add b o))

(defn ^:static chunk [^clojure.lang.chunk_buffer.ChunkBuffer b]
  (-chunk b))

(require '[clojure.lang.enumeration-seq :refer [make-enumeration-seq]])

(defn enumeration-seq [iter]
  (make-enumeration-seq iter))

(declare atom)
(declare reset!)
(require ['clojure.lang.lazy-seq :refer ['make-lazy-seq]])

(defmacro lazy-seq [& s-body]
  (list make-lazy-seq (list* 'clojure.core/fn [] s-body)))

(defn constantly [rval]
  (fn [& args] rval))

(defn take [n coll]
  (lazy-seq
    (when (pos? n)
      (when-let [s (seq coll)]
        (cons (first s) (take (dec n) (next s)))))))

(defn take-while [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (when (pred (first s))
        (cons (first s) (take-while pred (next s)))))))

(defn repeat
  ([x] (lazy-seq (cons x (repeat x))))
  ([n x] (take n (repeat x))))

(defn repeatedly
  ([f] (lazy-seq (cons (f) (repeatedly f))))
  ([n f] (take n (repeatedly f))))

(defn conj
  ([] [])
  ([coll] coll)
  ([coll x] (-cons coll x))
  ([coll x & xs]
   (if xs
     (recur (conj coll x) (first xs) (next xs))
     (conj coll x))))

(defn disj
  ([s] s)
  ([s x]
    (when s
      (-disj s x)))
  ([s x & xs]
    (when s
      (let [ret (-disj s x)]
        (if xs
          (recur ret (first xs) (next xs))
          ret)))))

(defn every? [pred s]
  (let [sq (seq s)]
    (cond
      (nil? s) true
      (pred (first sq)) (recur pred (next sq))
      :else false)))

(defn key [entry]
  (-key entry))

(defn val [entry]
  (-val entry))

(declare contains?)
(require ['clojure.lang.persistent-map :refer ['new-key-seq 'new-val-seq]])

(defn keys [m]
  (new-key-seq (seq m)))

(defn vals [m]
  (new-val-seq (seq m)))

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

(defn- dissoc-seq [m ks]
  (if ks
    (recur (-dissoc m (first ks)) (next ks))
    m))

(defn dissoc
  ([m] m)
  ([m k] (-dissoc m k))
  ([m k & ks]
   (dissoc-seq (-dissoc m k) (seq ks))))

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

(defn transient [coll]
  (-as-transient coll))

(defn persistent! [coll]
  (-persistent coll))

(defn conj! [coll x]
  (-conj! coll x))

(defn assoc! [coll index x]
  (-assoc! coll index x))

(defn dissoc! [coll index]
  (-dissoc! coll index))

(defn pop! [coll]
  (-pop! coll))

(require ['clojure.lang.persistent-vector :refer ['EMPTY-VECTOR 'is-chunked-seq? 'make-subvec]])

(defn vector [& args]
  (let [arg-seq (seq args)
        empty-transient (-as-transient EMPTY-VECTOR)]
    (if arg-seq
      (loop [xs arg-seq v empty-transient]
        (if xs
          (recur (next xs) (-conj! v (first xs)))
          (-persistent v)))
      (-persistent empty-transient))))

(defn subvec
  ([v start]
    (subvec v start (count v)))
  ([v start end]
    (cond
      (or (> start end) (< start 0) (> end (count v)))
        (throw (new-out-of-bounds-exception))
      (= start end)
        EMPTY-VECTOR
      :else
        (make-subvec v start end nil))))

(defn chunked-seq? [cs]
  (is-chunked-seq? cs))

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

(require ['clojure.lang.persistent-array-map :refer ['new-array-map]])

(defn array-map [& args]
  (let [sargs (seq args)
        size (count sargs)]
    (if (even? size)
      (new-array-map (into-array sargs) size (/ size 2) nil)
      (throw (new-argument-error
               (format "PersistentArrayMap can only be created with even number of arguments: %s arguments given"
                       size))))))

(require ['clojure.lang.apersistent-set :refer ['make-pairs]])
(require ['clojure.lang.persistent-hash-set :refer ['make-hash-set]])

(defn hash-set
  ([] (make-hash-set (hash-map)))
  ([& xs]
    (make-hash-set
      (apply hash-map (make-pairs xs)))))

(defn comparator [predicate]
  (fn [x y]
    (cond
      (predicate x y) -1
      (predicate y x) 1
      :else 0)))

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

(require ['clojure.lang.persistent-sorted-map :refer ['make-sorted-map]])

(defn sorted-map-by [compare-fn & args]
  (let [comparable (comparator compare-fn)]
    (make-sorted-map comparable args)))

(defn sorted-map [& args]
  (make-sorted-map compare args))

(require ['clojure.lang.persistent-sorted-set :refer ['make-sorted-set]])

(defn sorted-set [& ks]
  (make-sorted-set
    (apply sorted-map (make-pairs ks))))

(defn sorted-set-by [compare-fn & ks]
  (make-sorted-set
    (apply sorted-map-by (clojure.core/cons compare-fn (make-pairs ks)))))

(require ['clojure.lang.persistent-struct-map :refer ['make-def 'make-struct-map]])

(defn create-struct [& ks]
  (make-def ks))

(defmacro defstruct [n & ks]
  `(def ~n (create-struct ~@ks)))

(defn struct-map [d & inits]
  (let [[vs ext] (loop [is inits
                        vs (object-array (count (-get-keyslots d)))
                        ext EMPTY-HASH-MAP]
                   (if (empty? is)
                     [vs ext]
                     (if (< (count is) 2)
                       (throw (new-argument-error (str "No value supplied for key: " (first is))))
                       (let [k (first is)
                             v (second is)
                             e (get (-get-keyslots d) k)]
                          (if e
                            (do
                              (aset vs (dec e) v)
                              (recur (rest (rest is)) vs ext))
                            (recur (rest (rest is)) vs (assoc ext k v)))))))]
     (make-struct-map d vs ext nil)))

(defn struct [d & vs]
  (if (> (count vs) (count (-get-keyslots d)))
    (throw (new-argument-error "Too many arguments to struct constructor"))
    (let [v-arr (object-array (count (-get-keyslots d)))]
      (loop [idx 0
             v vs]
        (when v
          (aset v-arr idx (first v))
          (recur (inc idx) (next v))))
      (make-struct-map d v-arr EMPTY-HASH-MAP nil))))

(defn distinct [coll]
  (let [step (fn step [xs seen]
                 (lazy-seq
                  ((fn [xs seen]
                    (let [f (first xs)]
                      (when-let [s (seq xs)]
                        (if (contains? seen f)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen f)))))))
                   xs seen)))]
    (step coll (hash-set))))

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

(require ['clojure.lang.agent :refer ['new-agent 'agent-get-error 'agent-restart 'agent-set-error-handler 'agent-get-error-handler
                                      'action-release-pending-sends
                                      'pooled-executor 'solo-executor]])
(require ['clojure.lang.thread :as 'threading])

(def ^:dynamic *agent* nil)

(defn- binding-conveyor-fn [f]
  (let [frame (clojure.lang.Var/cloneThreadBindingFrame)]
    (fn
      ([]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f))
      ([x]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x))
      ([x y]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y))
      ([x y z]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y z))
      ([x y z & args]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (apply f x y z args)))))

(defn send-via [executor agnt f & args]
  (-dispatch agnt (binding [*agent* agnt] (binding-conveyor-fn f)) args executor))

(defn send [agnt f & args]
  (apply send-via pooled-executor agnt f args))

(defn send-off [agnt f & args]
  (apply send-via solo-executor agnt f args))

(defn release-pending-sends [] action-release-pending-sends)

(defn agent [state & args]
  (let [options (apply hash-map args)
        err-handler (get options :error-handler)]
    (new-agent state err-handler
               (get options :meta)
               (get options :validator)
               (get options :watches)
               (get options :error-mode
                 (if err-handler :continue :fail)))))

(defn agent-error [agnt]
  (agent-get-error agnt))

(defn agent-errors [agnt]
  (when-let [error (agent-error agnt)]
    (list error)))

(defn set-error-handler! [agnt error-fn]
  (agent-set-error-handler agnt error-fn))

(defn error-handler [agnt]
  (agent-get-error-handler agnt))

(defn restart-agent [agnt new-state & options]
  (let [opts (apply hash-map options)]
    (agent-restart agnt new-state opts)))

(defmacro io! [& body]
  (let [message (when (string? (first body)) (first body))
        body (if message (next body) body)]
    ; TODO stop relying on LockingTransaction
    `(if (clojure.lang.LockingTransaction/isRunning)
       (throw (new-argument-error ~(or message "I/O in transaction")))
       (do ~@body))))

(defn await [& agnts]
  (io! "await in transaction"
    (when *agent*
      (throw (new-exception "Can't wait in agent action")))
    (let [latch (threading/new-countdown-latch (clojure.core/count agnts))
          count-down (fn [agnt] (threading/latch-countdown latch) agnt)]
      (doseq [agnt agnts]
        (send agnt count-down))
      (threading/latch-await latch))))

(defn await-for [timeout-ms & agnts]
  (io! "await-for in transaction"
    (when *agent*
      (throw (new-exception "Can't wait in agent action")))
    (let [latch (threading/new-countdown-latch (clojure.core/count agnts))
          count-down (fn [agnt] (threading/latch-countdown latch) agnt)]
      (doseq [agnt agnts]
        (send agnt count-down))
      (threading/latch-await latch timeout-ms))))

(defn- setup-reference [reference options]
  (when-let [ref-meta (get options :meta)]
    (reset-meta! reference ref-meta))
  (when-let [validator (get options :validator)]
    (set-validator! reference validator))
    reference)

(require ['clojure.lang.atomic-ref :refer ['new-atomic-ref]])
(require ['clojure.lang.atom :refer ['new-atom]])

(defn atom
  ([state]
    (atom state :meta nil :validator nil))
  ([state & args]
    (let [config (apply array-map args)]
      (setup-reference
        (new-atom (new-atomic-ref state) nil nil (array-map))
        config))))

(defn memoize [f]
  (let [cache-atom (atom (hash-map))]
    (fn [& args]
      (let [cache (deref cache-atom)]
        (if (contains? cache args)
          (get cache args)
          (let [return-value (apply f args)]
            (swap! cache-atom assoc args return-value)
            return-value))))))

(require ['clojure.lang.future :refer         ['new-future]])
(require ['clojure.lang.future-submission :as 'future-submission])

(defn future-call [f]
  (let [fun (binding-conveyor-fn f)]
    (new-future fun)))

(defmacro future [& body]
  `(future-call (^{:once true} fn* [] ~@body)))

(defn future? [f]
  (future-submission/is-future? f))

(defn future-cancel [f]
  (future-submission/cancel f true))

(defn future-cancelled? [f]
  (future-submission/is-cancelled? f))

(defn future-done? [f]
  (future-submission/is-done? f))

(require ['clojure.lang.hash :refer ['hash-combine]])
(require ['clojure.lang.show :refer ['build-string]])
(require ['clojure.lang.symbol :as 'sym])

(defn str
  ([] "")
  ([x]
   (if (nil? x) "" (-show x)))
  ([x & more]
   (build-string (cons x more))))

(defn symbol? [x]
  (sym/is-symbol? x))

(defn symbol
  ([name]
   (if (symbol? name)
     name
     (let [parts (clojure.string/split name #"/")]
       (if (= 1 (clojure.core/count parts))
         (symbol nil (clojure.core/first parts))
         (symbol (clojure.string/join "/" (butlast parts)) (clojure.core/last parts))))))
  ([ns name]
   (if (nil? name)
     (throw (Exception. "Can't create symbol with nil name")))
   (sym/new-symbol ns name (if ns (str ns "/" name) name)
               (hash (hash-combine (hash name) (hash ns))) nil)))

(require ['clojure.lang.atomic-counter :refer ['new-atomic-counter 'get-and-increment-atomic-counter]])

(def ^:private gensym-counter (new-atomic-counter 1))
(defn- next-gensym-value [] (get-and-increment-atomic-counter gensym-counter))

(defn gensym
  ([] (gensym "G__"))
  ([prefix] (symbol (str prefix (next-gensym-value)))))

(require ['clojure.lang.keyword :as 'kwd])

(defn keyword? [x]
  (kwd/is-keyword? x))

(defn keyword
  ([n]
   (let [sym (symbol n)]
     (keyword (namespace sym) (name sym))))
  ([ns name]
   (let [sym (symbol ns name)
         hash-code (hash (clojure.core/+ (hash sym) 0x9e3779b9))]
     (kwd/new-keyword ns name (str ":" sym) hash-code {} sym))))

(defmacro when-not [test & body]
  (list 'if test nil (clojure.core/cons 'do body)))

(require '[clojure.lang.regex :as regex])

(defn re-pattern [pattern]
  (regex/make-pattern pattern))

(defn re-matcher [pattern s]
  (regex/make-matcher pattern s))

(defn re-groups [matcher]
  (regex/get-groups matcher))

(defn re-find
  ([matcher] (regex/regex-find matcher))
  ([pattern s]
    (regex/regex-find (re-matcher pattern s))))

(defn re-seq [pattern s]
  (regex/regex-seq pattern s))

(defmacro assert
  ([assertion]
    (when *assert*
      `(when-not ~assertion
         (throw (new-assertion-error (str "Assert failed: " (pr-str '~assertion)))))))
  ([assertion message]
    (when *assert*
      `(when-not ~assertion
         (throw (new-assertion-error (str "Assert failed: " ~message "\n" (pr-str '~assertion))))))))

(defmacro while [test & body]
  `(loop []
     (when ~test
       `@body
       (recur))))

(defn test [v]
  (if-let [f (get (meta v) :test)]
    (do (f) :ok)
    :no-test))

(defmacro ^:private assert-args [& pairs]
  `(do
    (when-not ~(first pairs)
      (throw (new-argument-error
               (str (first ~'&form) " requires " ~(second pairs) " in " ~'clojure.core/*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(def ^:dynamic *print-dup* false)
(def ^:dynamic *print-meta* false)
(def ^:dynamic *print-readably* true)
(def ^:dynamic *print-level* nil)
(def ^:dynamic *print-length* nil)
(def ^:dynamic *flush-on-newline* true)
(declare pr print-ctor)

(def char-escape-string
  (array-map
    \newline   "\\n"
    \tab       "\\t"
    \return    "\\r"
    \"         "\\\""
    \\         "\\\\"
    \formfeed  "\\f"
    \backspace "\\b"))

(def char-name-string
  (array-map
    \newline "newline"
    \tab "tab"
    \space "space"
    \backspace "backspace"
    \formfeed "formfeed"
    \return "return"))

(defmulti print-method (fn [obj writer]
                         (let [t (get (meta obj) (keyword "type"))]
                           (if (keyword? t) t (class obj)))))

(defmulti print-dup (fn [obj writer] (class obj)))

(require ['clojure.lang.input-output :refer :all])
(def ^:dynamic *out* (default-out))

(defn line-seq [rdr]
  (when-let [line (platform-read-line rdr)]
    (cons line (lazy-seq (line-seq rdr)))))

(defn newline []
  (platform-newline)
  nil)

(defn flush []
  (platform-flush)
  nil)

(defn print-ctor [obj print-args wrtr]
  (platform-print-constructor obj print-args wrtr))

(defn print-simple [obj wrtr]
  (print-meta obj wrtr)
  (platform-write wrtr (str obj)))

(defmethod print-method :default [obj wrtr]
  (print-simple obj wrtr))

(defmethod print-method nil [obj wrtr]
  (platform-write wrtr "nil"))

(defmethod print-method clojure.lang.keyword.Keyword [obj wrtr]
  (platform-write wrtr (str obj)))

(defmethod print-method clojure.lang.symbol.Symbol [obj wrtr]
  (print-simple obj wrtr))

(defmethod print-method clojure.lang.protocols.ISeq [obj wrtr]
  (print-meta obj wrtr)
  (print-sequential "(" print-method " " ")" obj wrtr))

(defmethod print-method clojure.lang.protocols.IPersistentMap [obj wrtr]
  (print-meta obj wrtr)
  (print-map obj print-method wrtr))

(defmethod print-method clojure.lang.protocols.IPersistentVector [obj wrtr]
  (print-meta obj wrtr)
  (print-sequential "[" print-method " " "]" obj wrtr))

(defmethod print-method clojure.lang.protocols.IPersistentSet [obj wrtr]
  (print-meta obj wrtr)
  (print-sequential "#{" print-method " " "}" obj wrtr))

(defmethod print-dup nil [obj wrtr]
  (print-method obj wrtr))

(defmethod print-dup clojure.lang.keyword.Keyword [obj wrtr]
  (print-method obj wrtr))

(defmethod print-dup clojure.lang.symbol.Symbol [obj wrtr]
  (print-method obj wrtr))

(defmethod print-dup clojure.lang.protocols.ISeq [obj wrtr]
  (print-method obj wrtr))

(defmethod print-dup clojure.lang.protocols.IPersistentList [obj wrtr]
  (print-method obj wrtr))

(defn pr-on [o w]
  (if *print-dup*
    (print-dup o w)
    (print-method o w))
  nil)

(defn pr
  ([] nil)
  ([obj]
    (pr-on obj *out*))
  ([x & more]
    (pr x)
    (platform-append-space *out*)
    (if-let [nmore (next more)]
      (recur (first more) nmore)
      (apply pr more))))

(defn prn [& more]
  (apply pr more)
  (platform-newline)
  (when *flush-on-newline*
    (platform-flush)))

(defn print [& more]
  (binding [*print-readably* nil]
    (apply pr more)))

(defn println [& more]
  (binding [*print-readably* nil]
    (apply prn more)))

(defmacro with-out-str [& body]
  `(let [o# (platform-out-str)]
    (binding [*out* o#]
      ~@body
      (str o#))))

(defmacro with-open [bindings & body]
  (assert-args
     (clojure.core/vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0)
      `(do ~@body)
    (clojure.core/symbol? (bindings 0))
      `(let ~(clojure.core/subvec bindings 0 2)
         (try
           (with-open ~(clojure.core/subvec bindings 2) ~@body)
           (finally
             (. ~(bindings 0) close))))
    :else
      (throw (new-argument-error "with-open only allows Symbols in bindings"))))

(require ['clojure.lang.time :refer ['nano-time]])

(defmacro time [expr]
  `(let [start# (nano-time)
         ret# ~expr]
     (prn (str "Elapsed time: " (/ (double (- (nano-time) start#)) 1000000.0) " msecs"))
     ret#))

(require '[clojure.lang.persistent-list :refer [EMPTY-LIST]])

(extend-type nil
  ISeqable
  (-seq [this] nil)
  ISeq
  (-first [this] nil)
  (-next [this] nil)
  (-more [this] EMPTY-LIST)
  IIndexed
  (-nth
    ([this n] nil)
    ([this n default] default)))


