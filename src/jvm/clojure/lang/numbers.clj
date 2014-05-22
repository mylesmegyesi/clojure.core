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

(defprotocol NumberCoercions
  (->byte   [this])
  (->short  [this])
  (->int    [this])
  (->long   [this])
  (->float  [this])
  (->double [this])
  (->bigint [this])
  (->ratio  [this])
  (->bigdec [this])
  (unsafe-cast-int [this]))

(extend-type Ratio
  IRatio
  (-numerator [this] (.getNumerator this))
  (-denominator [this] (.getDenominator this))

  NumberCoercions
  (->ratio  [this] (.ratioValue this))
  (->int    [this] (.intValue this))
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->bigint [this] (.bigIntegerValue this))
  (->bigdec [this] (.bigDecimalValue this)))

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

(extend-type Byte
  NumberCoercions
  (->byte   [this] this)
  (->short  [this] (.shortValue this))
  (->int    [this] (.intValue this))
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->ratio  [this] (make-ratio this BigInteger/ONE))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. (.intValue this))))

(extend-type Short
  NumberCoercions
  (->short  [this] this)
  (->int    [this] (.intValue this))
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->ratio  [this] (make-ratio this BigInteger/ONE))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. (.intValue this))))

(extend-type Integer
  NumberCoercions
  (->int           [this] this)
  (->long          [this] (.longValue this))
  (->float         [this] (.floatValue this))
  (->double        [this] (.doubleValue this))
  (->ratio         [this] (make-ratio this BigInteger/ONE))
  (->bigint        [this] (BigInteger. (.toString this)))
  (->bigdec        [this] (BigDecimal. this))
  (unsafe-cast-int [this] this))

(extend-type AtomicInteger
  NumberCoercions
  (->int    [this] (.intValue this))
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->ratio  [this] (make-ratio this BigInteger/ONE))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. (.intValue this))))

(extend-type Long
  NumberCoercions
  (->long          [this] this)
  (->float         [this] (.floatValue this))
  (->double        [this] (.doubleValue this))
  (->ratio         [this] (make-ratio this BigInteger/ONE))
  (->bigint        [this] (BigInteger. (.toString this)))
  (->bigdec        [this] (BigDecimal. (.longValue this)))
  (unsafe-cast-int [this] (.intValue this)))

(extend-type AtomicLong
  NumberCoercions
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->ratio  [this] (make-ratio this BigInteger/ONE))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. (.longValue this))))

(extend-type Float
  NumberCoercions
  (->float  [this] this)
  (->double [this] (.doubleValue this))
  (->bigdec [this] (BigDecimal. (.doubleValue this))))

(extend-type Double
  NumberCoercions
  (->double [this] this)
  (->bigdec [this] (BigDecimal. this)))

(extend-type BigInteger
  NumberCoercions
  (->ratio  [this] (make-ratio this BigInteger/ONE))
  (->double [this] (.doubleValue this))
  (->bigint [this] this)
  (->bigdec [this] (BigDecimal. this)))

(extend-type clojure.lang.BigInt
  NumberCoercions
  (->ratio  [this] (make-ratio this BigInteger/ONE))
  (->double [this] (.doubleValue this))
  (->bigint [this] (.toBigInteger this))
  (->bigdec [this] (BigDecimal. (.toBigInteger this))))

(extend-type BigDecimal
  NumberCoercions
  (->bigdec [this] this)
  (->double [this] (.doubleValue this)))

(defprotocol Ops
  (ops-bit-count                [ops i])
  (ops-zero?                    [ops x]))

(deftype IntegerOps []
  Ops
  (ops-bit-count                 [_ i]   (Integer/bitCount i)))

(def INTEGER-ZERO (Integer. "0"))

(deftype BigDecimalOps []
  Ops
  (ops-zero?  [this x]
    (.equals (.signum x) INTEGER-ZERO)))

(def INTEGER-OPS (IntegerOps.))
(def BIGDECIMAL-OPS (BigDecimalOps.))

(defprotocol MakeOps
  (make-ops [this]))

(extend-protocol MakeOps
  Integer
  (make-ops [this] INTEGER-OPS)

  BigDecimal
  (make-ops [this] BIGDECIMAL-OPS))

(defmulti no-overflow-ops (fn [t1 t2] [t1 t2]))

(defmethod no-overflow-ops [Byte Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Byte AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Byte BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Short Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Short AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Short BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Integer Byte]          [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer Short]         [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Integer Ratio]         [ops1 ops2] INTEGER-OPS)

(defmethod no-overflow-ops [AtomicInteger Byte]          [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger Short]         [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [AtomicInteger Ratio]         [ops1 ops2] INTEGER-OPS)

(defmethod no-overflow-ops [Long BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [AtomicLong BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Float Ratio]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float BigInt]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Double BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Double BigInt]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Double BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Ratio Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Ratio AtomicInteger] [ops1 ops2] INTEGER-OPS)
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

(defprotocol BitOperations
  (-bit-count                [this]))

(defn- long-hash-code [lpart]
  (unsafe-cast-int
    (bxor
      lpart
      (bunsigned-shift-right lpart 32))))

(extend-protocol IHash
  Byte
  (-hash [this]
    (long-hash-code (->long this)))

  Short
  (-hash [this]
    (long-hash-code (->long this)))

  Integer
  (-hash [this]
    (long-hash-code (->long this)))

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

(extend-type Number
  BitOperations
  (-bit-count [i]
    (ops-bit-count (make-ops i) i))

  NumberCoercions
  (->long   [this] (.longValue this))
  (->double [this] (.doubleValue this))
  (->bigint [this] (. BigInteger (valueOf (.longValue this))))
  (->ratio  [this] (make-ratio (->bigint this) BigInteger/ONE))
  (->bigdec [this] (. BigDecimal (valueOf (.longValue this))))

  )
