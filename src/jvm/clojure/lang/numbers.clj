(ns clojure.lang.numbers
  (:refer-clojure :only [and or case cond contains? defmacro defn defn- defprotocol deftype defmulti defmethod defn- mod not nil? zero? extend-protocol extend-type fn let -> / =])
  (:require [clojure.lang.protocols :refer [IHash IRatio -denominator -numerator]]
            [clojure.next           :refer [instance? type]])
  (:import [java.lang Number Short Byte Integer Long Float Double]
           [java.math BigInteger BigDecimal]
           [java.util.concurrent.atomic AtomicInteger AtomicLong]
           [clojure.lang BigInt]
           [clojure.lang.platform NumberOps]
           [clojure.lang.platform Ratio]
           [clojure.lang.platform.numbers Equivalence]
           [clojure.lang.platform.numbers BitOps]
           [clojure.lang.platform.numbers Addition]
           [clojure.lang.platform.numbers Increment]
           [clojure.lang.platform.numbers Multiplication]
           [clojure.lang.platform.numbers Subtraction]
           [clojure.lang.platform.numbers Decrement]
           [clojure.lang.platform.numbers Division]
           [clojure.lang.platform.numbers Negation]))

(defmacro band [x y]
  `(. BitOps (numberBitAnd ~x ~y)))

(defmacro bor [x y]
  `(. BitOps (numberBitOr ~x ~y)))

(defmacro bxor [x y]
  `(. BitOps (numberBitXor ~x ~y)))

(defmacro bshift-left [x y]
  `(. BitOps (numberBitShiftLeft ~x ~y)))

(defmacro bunsigned-shift-right [x y]
  `(. BitOps (numberUnsignedBitShiftRight ~x ~y)))

(defmacro add [x y]
  `(. Addition (numberAdd ~x ~y)))

(defmacro increment [x]
  `(. Increment (numberIncrement ~x)))

(defmacro multiply [x y]
  `(. Multiplication (numberMultiply ~x ~y)))

(defmacro subtract
  ([x] `(. Negation (numberNegate ~x)))
  ([x y] `(. Subtraction (numberSubtract ~x ~y))))

(defmacro decrement [x]
  `(. Decrement (numberDecrement ~x)))

(defmacro divide [x y]
  `(. Division (numberDivide ~x ~y)))

(defmacro numbers-equal? [x y]
  `(. Equivalence (numberEqual ~x ~y)))

(defmacro numbers-equivalent? [x y]
  `(. Equivalence (numbersEquivalent ~x ~y)))

(defn ->int [n]
  (.intValue n))

(extend-type Ratio
  IRatio
  (-numerator [this] (.getNumerator this))
  (-denominator [this] (.getDenominator this)))

(defn- gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn make-ratio [numerator denominator]
 (let [gcd (gcd numerator denominator)]
    (if (zero? gcd)
      (Ratio. (BigInteger. "0") (BigInteger. "1"))
      (let [n (BigInteger. (.toString (/ numerator gcd)))
            d (BigInteger. (.toString (/ denominator gcd)))]
        (Ratio. n d)))))

(defprotocol Ops
  (ops-zero?                    [ops x]))

(def INTEGER-ZERO (Integer. "0"))

(deftype BigDecimalOps []
  Ops
  (ops-zero?  [this x]
    (.equals (.signum x) INTEGER-ZERO)))

(def BIGDECIMAL-OPS (BigDecimalOps.))

(defprotocol MakeOps
  (make-ops [this]))

(extend-protocol MakeOps
  BigDecimal
  (make-ops [this] BIGDECIMAL-OPS))

(defmulti no-overflow-ops (fn [t1 t2] [t1 t2]))

(defmethod no-overflow-ops [Byte BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Short BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Integer BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [AtomicInteger BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Long BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [AtomicLong BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float Ratio]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float BigInt]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Double BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Double BigInt]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Double BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Ratio Float]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Ratio Double]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Ratio BigInt]        [ops2 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Ratio BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Ratio BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger Float]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger Double]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger BigInt]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger Ratio]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInt Float]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInt Double]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInt BigInt]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInt BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInt BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInt Ratio]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Byte]          [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Short]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Integer]       [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal AtomicInteger] [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Long]          [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal AtomicLong]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Float]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Double]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Ratio]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal BigInt]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defn unsafe-cast-int [i]
  (.intValue i))

(defn- long-hash-code [lpart]
  (unsafe-cast-int
    (bxor
      lpart
      (bunsigned-shift-right lpart 32))))

(extend-protocol IHash
  Byte
  (-hash [this]
    (long-hash-code (.longValue this)))

  Short
  (-hash [this]
    (long-hash-code (.longValue this)))

  Integer
  (-hash [this]
    (long-hash-code (.longValue this)))

  Long
  (-hash [this]
    (long-hash-code this))

  BigDecimal
  (-hash [this]
    (if (ops-zero? (make-ops this) this)
      (.hashCode BigInteger/ZERO)
      (.hashCode (.stripTrailingZeros this))))

  Ratio
  (-hash [this] (.hashCode this))

  )

