(ns clojure.lang.numbers
  (:refer-clojure :only [cond let fn defmacro defn defn- mod extend-protocol extend-type])
  (:require [clojure.lang.protocols :refer [IDecimal IFloat IHash IInteger IRatio
                                            -denominator -numerator]]
            [clojure.next           :refer :all])
  (:import [java.lang Number Short Byte Integer Long Float Double]
           [java.math BigInteger BigDecimal]
           [clojure.lang BigInt]
           [clojure.lang.platform Ratio]
           [clojure.lang.platform.numbers Addition BitOps Cast Comparison
                                          Decrement Division Equivalence
                                          Increment Multiplication Negation
                                          Quotient Remainder Subtraction
                                          Zero]))

(def platform-byte        Byte)
(def platform-short       Short)
(def platform-int         Integer)
(def platform-long        Long)
(def platform-big-int     BigInt)
(def platform-big-integer BigInteger)
(def platform-float       Float)
(def platform-double      Double)

(defmacro is-number? [x]
  `(instance? Number ~x))

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

(defmacro bclear [x y]
  `(. BitOps (numberBitClear ~x ~y)))

(defmacro bset [x y]
  `(. BitOps (numberBitSet ~x ~y)))

(defmacro bflip [x y]
  `(. BitOps (numberBitFlip ~x ~y)))

(defmacro btest [x y]
  `(. BitOps (numberBitTest ~x ~y)))

(defmacro bshift-left [x y]
  `(. BitOps (numberBitShiftLeft ~x ~y)))

(defmacro bshift-right [x y]
  `(. BitOps (numberBitShiftRight ~x ~y)))

(defmacro bunsigned-shift-right [x y]
  `(. BitOps (numberUnsignedBitShiftRight ~x ~y)))

(defmacro add [x y]
  `(. Addition (numberAdd ~x ~y)))

(defmacro addp [x y]
  `(. Addition (numberPrecisionAdd ~x ~y)))

(defmacro increment [x]
  `(. Increment (numberIncrement ~x)))

(defmacro incrementp [x]
  `(. Increment (numberPrecisionIncrement ~x)))

(defmacro multiply [x y]
  `(. Multiplication (numberMultiply ~x ~y)))

(defmacro multiplyp [x y]
  `(. Multiplication (numberPrecisionMultiply ~x ~y)))

(defmacro subtract
  ([x] `(. Negation (numberNegate ~x)))
  ([x y] `(. Subtraction (numberSubtract ~x ~y))))

(defmacro subtractp
  ([x] `(. Negation (numberPrecisionNegate ~x)))
  ([x y] `(. Subtraction (numberPrecisionSubtract ~x ~y))))

(defmacro decrement [x]
  `(. Decrement (numberDecrement ~x)))

(defmacro decrementp [x]
  `(. Decrement (numberPrecisionDecrement ~x)))

(defmacro maximum [x y]
  `(. Comparison (numberMaximum ~x ~y)))

(defmacro divide [x y]
  `(. Division (numberDivide ~x ~y)))

(defmacro quotient [x y]
  `(. Quotient (numberQuotient ~x ~y)))

(defmacro remainder [x y]
  `(. Remainder (numberRemainder ~x ~y)))

(defmacro numbers-equal? [x y]
  `(. Equivalence (numberEqual ~x ~y)))

(defmacro numbers-equivalent? [x y]
  `(. Equivalence (numbersEquivalent ~x ~y)))

(defmacro is-zero? [x]
  `(. Zero (numberIsZero ~x)))

(defmacro ->byte [x]
  `(. Cast (castToByte ~x)))

(defmacro ->short [x]
  `(. Cast (castToShort ~x)))

(defmacro ->int [x]
  `(. Cast (castToInt ~x)))

(defmacro ->long [x]
  `(. Cast (castToLong ~x)))

(defmacro ->float [x]
  `(. Cast (castToFloat ~x)))

(defmacro ->double [x]
  `(. Cast (castToDouble ~x)))

(defmacro ->bigint [x]
  `(let [x# ~x]
     (cond
       (instance? BigInt x#) x#
       (instance? BigInteger x#) (BigInt/fromBigInteger x#)
       (decimal? x#) (bigint (.toBigInteger ^BigDecimal x#))
       (float? x#) (bigint (. BigDecimal valueOf (->double x#)))
       (ratio? x#) (bigint (.bigIntegerValue ^Ratio x#))
       (clojure.core/number? x#) (BigInt/valueOf (->long x#))
       :else (bigint (BigInteger. x#)))))

(defmacro ->biginteger [x]
  `(let [x# ~x]
     (cond
       (instance? BigInteger x#) x#
       (instance? BigInt x#) (.toBigInteger ^BigInt x#)
       (decimal? x#) (.toBigInteger ^BigDecimal x#)
       (float? x#) (.toBigInteger (. BigDecimal valueOf (->double x#)))
       (ratio? x#) (.bigIntegerValue ^Ratio x#)
       (clojure.core/number? x#) (BigInteger/valueOf (->long x#))
       :else (BigInteger. x#))))

(defmacro ->bigdec [x]
  `(let [x# ~x]
     (cond
       (decimal? x#) x#
       (float? x#) (. BigDecimal valueOf (->double x#))
       (ratio? x#) (divide (BigDecimal. (.getNumerator ^Ratio x#)) (.getDenominator ^Ratio x#))
       (instance? BigInt x#) (.toBigDecimal ^BigInt x#)
       (instance? BigInteger x#) (BigDecimal. ^BigInteger x#)
       (clojure.core/number? x#) (BigDecimal/valueOf (->long x#))
       :else (BigDecimal. x#))))

(defmacro lt [x y]
  `(. Comparison (lessThan ~x ~y)))

(defmacro lte [x y]
  `(. Comparison (lessThanEqualTo ~x ~y)))

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

(extend-protocol IInteger
  Short Byte Integer Long BigInteger BigInt)

(extend-protocol IFloat
  Float Double)

(extend-protocol IDecimal
  BigDecimal)

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
  (-hash [this] (.hashCode this)))

