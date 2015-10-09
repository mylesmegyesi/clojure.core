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
                                          Quotient Rationalize Remainder
                                          Subtraction Zero]))

(def platform-byte          Byte)
(def platform-native-byte   Byte/TYPE)
(def platform-short         Short)
(def platform-native-short  Short/TYPE)
(def platform-int           Integer)
(def platform-native-int    Integer/TYPE)
(def platform-long          Long)
(def platform-native-long   Long/TYPE)
(def platform-big-int       BigInt)
(def platform-big-integer   BigInteger)
(def platform-float         Float)
(def platform-native-float  Float/TYPE)
(def platform-double        Double)
(def platform-native-double Double/TYPE)

(defmacro number? [x]
  `(instance? Number ~x))

(defmacro bit-not [x]
  `(. BitOps (numberBitNot ~x)))

(defmacro bit-and [x y]
  `(. BitOps (numberBitAnd ~x ~y)))

(defmacro bit-and-not [x y]
  `(. BitOps (numberBitAndNot ~x ~y)))

(defmacro bit-or [x y]
  `(. BitOps (numberBitOr ~x ~y)))

(defmacro bit-xor [x y]
  `(. BitOps (numberBitXor ~x ~y)))

(defmacro bit-clear [x y]
  `(. BitOps (numberBitClear ~x ~y)))

(defmacro bit-set [x y]
  `(. BitOps (numberBitSet ~x ~y)))

(defmacro bit-flip [x y]
  `(. BitOps (numberBitFlip ~x ~y)))

(defmacro bit-test [x y]
  `(. BitOps (numberBitTest ~x ~y)))

(defmacro bit-shift-left [x y]
  `(. BitOps (numberBitShiftLeft ~x ~y)))

(defmacro bit-shift-right [x y]
  `(. BitOps (numberBitShiftRight ~x ~y)))

(defmacro unsigned-bit-shift-right [x y]
  `(. BitOps (numberUnsignedBitShiftRight ~x ~y)))

(defmacro + [x y]
  `(. Addition (numberAdd ~x ~y)))

(defmacro +' [x y]
  `(. Addition (numberPrecisionAdd ~x ~y)))

(defmacro inc [x]
  `(. Increment (numberIncrement ~x)))

(defmacro unchecked-inc [x]
  `(. Increment (numberUncheckedIncrement ~x)))

(defmacro inc' [x]
  `(. Increment (numberPrecisionIncrement ~x)))

(defmacro * [x y]
  `(. Multiplication (numberMultiply ~x ~y)))

(defmacro *' [x y]
  `(. Multiplication (numberPrecisionMultiply ~x ~y)))

(defmacro unchecked-multiply [x y]
  `(. Multiplication (numberUncheckedMultiply ~x ~y)))

(defmacro unchecked-multiply-int [x y]
  `(. Multiplication (numberUncheckedMultiplyInt ~x ~y)))

(defmacro -
  ([x] `(. Negation (numberNegate ~x)))
  ([x y] `(. Subtraction (numberSubtract ~x ~y))))

(defmacro -'
  ([x] `(. Negation (numberPrecisionNegate ~x)))
  ([x y] `(. Subtraction (numberPrecisionSubtract ~x ~y))))

(defmacro dec [x]
  `(. Decrement (numberDecrement ~x)))

(defmacro unchecked-dec [x]
  `(. Decrement (numberUncheckedDecrement ~x)))

(defmacro dec' [x]
  `(. Decrement (numberPrecisionDecrement ~x)))

(defmacro max [x y]
  `(. Comparison (numberMaximum ~x ~y)))

(defmacro min [x y]
  `(. Comparison (numberMinimum ~x ~y)))

(defmacro / [x y]
  `(. Division (numberDivide ~x ~y)))

(defmacro quot [x y]
  `(. Quotient (numberQuotient ~x ~y)))

(defmacro rem [x y]
  `(. Remainder (numberRemainder ~x ~y)))

(defmacro rationalize [x]
  `(. Rationalize (numberRationalize ~x)))

(defmacro equal? [x y]
  `(. Equivalence (numberEqual ~x ~y)))

(defmacro equivalent? [x y]
  `(. Equivalence (numbersEquivalent ~x ~y)))

(defmacro zero? [x]
  `(. Zero (numberIsZero ~x)))

(defmacro ->byte [x]
  `(. Cast (castToByte ~x)))

(defmacro unchecked->byte [x]
  `(. Cast (uncheckedCastToByte ~x)))

(defmacro ->short [x]
  `(. Cast (castToShort ~x)))

(defmacro unchecked->short [x]
  `(. Cast (uncheckedCastToShort ~x)))

(defmacro ->int [x]
  `(. Cast (castToInt ~x)))

(defmacro unchecked->int [x]
  `(. Cast (uncheckedCastToInt ~x)))

(defmacro ->long [x]
  `(. Cast (castToLong ~x)))

(defmacro unchecked->long [x]
  `(. Cast (uncheckedCastToLong ~x)))

(defmacro ->float [x]
  `(. Cast (castToFloat ~x)))

(defmacro unchecked->float [x]
  `(. Cast (uncheckedCastToFloat ~x)))

(defmacro ->double [x]
  `(. Cast (castToDouble ~x)))

(defmacro unchecked->double [x]
  `(. Cast (uncheckedCastToDouble ~x)))

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
       (ratio? x#) (/ (BigDecimal. (.getNumerator ^Ratio x#)) (.getDenominator ^Ratio x#))
       (instance? BigInt x#) (.toBigDecimal ^BigInt x#)
       (instance? BigInteger x#) (BigDecimal. ^BigInteger x#)
       (clojure.core/number? x#) (BigDecimal/valueOf (->long x#))
       :else (BigDecimal. x#))))

(defmacro < [x y]
  `(. Comparison (lessThan ~x ~y)))

(defmacro <= [x y]
  `(. Comparison (lessThanEqualTo ~x ~y)))

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

(defn unsafe-cast-int [i]
  (.intValue i))

(defn- long-hash-code [lpart]
  (unsafe-cast-int
    (bit-xor
      lpart
      (unsigned-bit-shift-right lpart 32))))

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
    (if (zero? this)
      (.hashCode BigInteger/ZERO)
      (.hashCode (.stripTrailingZeros this))))

  Ratio
  (-hash [this] (.hashCode this)))

