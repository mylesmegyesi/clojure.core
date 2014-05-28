(ns clojure.lang.numbers
  (:refer-clojure :only [let fn defmacro defn defn- mod extend-protocol extend-type])
  (:require [clojure.lang.protocols :refer [IHash IRatio -denominator -numerator]]
            [clojure.next           :refer :all])
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
           [clojure.lang.platform.numbers Negation]
           [clojure.lang.platform.numbers Zero]))

(defmacro bnot [x]
  `(. BitOps (numberBitNot ~x)))

(defmacro band [x y]
  `(. BitOps (numberBitAnd ~x ~y)))

(defmacro band-not [x y]
  `(. BitOps (numberBitAndNot ~x ~y)))

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

(defmacro is-zero? [x]
  `(. Zero (numberIsZero ~x)))

(defn ->int [n]
  (.intValue n))

(extend-type Ratio
  IRatio
  (-numerator [this] (.getNumerator this))
  (-denominator [this] (.getDenominator this)))

(defn- gcd [a b]
  (if (is-zero? b)
    a
    (recur b (mod a b))))

(defn make-ratio [numerator denominator]
 (let [gcd (gcd numerator denominator)]
    (if (is-zero? gcd)
      (Ratio. (BigInteger. "0") (BigInteger. "1"))
      (let [n (BigInteger. (.toString (divide numerator gcd)))
            d (BigInteger. (.toString (divide denominator gcd)))]
        (Ratio. n d)))))

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
    (if (is-zero? this)
      (.hashCode BigInteger/ZERO)
      (.hashCode (.stripTrailingZeros this))))

  Ratio
  (-hash [this] (.hashCode this))

  )

