(ns clojure.lang.platform.numbers
  (:refer-clojure :only [and or defmacro defn defn- defprotocol deftype defmulti defmethod defn- mod not nil? zero? extend-protocol extend-type fn let -> not= = /])
  (:require [clojure.lang.object    :refer [type instance?]]
            [clojure.lang.protocols :refer [IEquivalence -equivalent? IHash IRatio]]
            [clojure.lang.ratio     :refer [denominator numerator]])
  (:import [java.lang Number Short Byte Integer Long Float Double]
           [java.math BigInteger BigDecimal]
           [java.util.concurrent.atomic AtomicInteger AtomicLong]
           [clojure.lang BigInt]
           [clojure.lang.platform NumberOps]))

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

(defprotocol Categorized
  (category [this]))

(deftype Ratio [-numerator -denominator]
  Object
  (equals [this other]
    (if (and (not (nil? other)) (instance? Ratio other))
      (and
        (.equals -numerator (numerator other))
        (.equals -denominator (denominator other)))
    false))

  IRatio
  (-numerator   [this] -numerator)
  (-denominator [this] -denominator)

  Categorized
  (category [this] :ratio)

  NumberCoercions
  (->ratio  [this] this)
  (->int    [this] (.intValue (->double this)))
  (->long   [this] (.longValue (->bigint this)))
  (->float  [this] (.floatValue (->double this)))
  (->double [this]
    (let [mc java.math.MathContext/DECIMAL64
          big-decimal-numerator (BigDecimal. -numerator)
          big-decimal-denominator (BigDecimal. -denominator)]
      (.doubleValue (.divide big-decimal-numerator big-decimal-denominator mc))))
  (->bigint [this] (.divide -numerator -denominator))
  (->bigdec [this]
    (let [mc java.math.MathContext/UNLIMITED
          big-decimal-numerator (BigDecimal. -numerator)
          big-decimal-denominator (BigDecimal. -denominator)]
      (.divide big-decimal-numerator big-decimal-denominator mc))))

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
  Categorized
  (category [this] :integer)

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
  Categorized
  (category [this] :integer)

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
  Categorized
  (category [this] :integer)

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
  Categorized
  (category [this] :integer)

  NumberCoercions
  (->int    [this] (.intValue this))
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->ratio  [this] (make-ratio this BigInteger/ONE))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. (.intValue this))))

(extend-type Long
  Categorized
  (category [this] :integer)

  NumberCoercions
  (->long          [this] this)
  (->float         [this] (.floatValue this))
  (->double        [this] (.doubleValue this))
  (->ratio         [this] (make-ratio this BigInteger/ONE))
  (->bigint        [this] (BigInteger. (.toString this)))
  (->bigdec        [this] (BigDecimal. (.longValue this)))
  (unsafe-cast-int [this] (.intValue this))
  )

(extend-type AtomicLong
  Categorized
  (category [this] :integer)

  NumberCoercions
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->ratio  [this] (make-ratio this BigInteger/ONE))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. (.longValue this))))

(extend-type Float
  Categorized
  (category [this] :floating)

  NumberCoercions
  (->float  [this] this)
  (->double [this] (.doubleValue this))
  (->bigdec [this] (BigDecimal. (.doubleValue this))))

(extend-type Double
  Categorized
  (category [this] :floating)

  NumberCoercions
  (->double [this] this)
  (->bigdec [this] (BigDecimal. this)))

(extend-type BigInteger
  Categorized
  (category [this] :integer)

  NumberCoercions
  (->ratio  [this] (make-ratio this BigInteger/ONE))
  (->bigint [this] this)
  (->bigdec [this] (BigDecimal. this)))

(extend-type clojure.lang.BigInt
  Categorized
  (category [this] :integer)

  NumberCoercions
  (->ratio  [this] (make-ratio this BigInteger/ONE))
  (->bigint [this] (.toBigInteger this))
  (->bigdec [this] (BigDecimal. (.toBigInteger this))))

(extend-type BigDecimal
  Categorized
  (category [this] :decimal)

  NumberCoercions
  (->bigdec [this] this))

(defprotocol Ops
  (ops-add                      [ops x y])
  (ops-bit-and                  [ops x y])
  (ops-bit-count                [ops i])
  (ops-bit-or                   [ops x y])
  (ops-bit-xor                  [ops x y])
  (ops-bit-shift-left           [ops x y])
  (ops-bit-unsigned-shift-right [ops x y])
  (ops-decrement                [ops i])
  (ops-equals                   [ops x y])
  (ops-increment                [ops i])
  (ops-multiply                 [ops x y])
  (ops-subtract                 [ops x y])
  (ops-zero?                    [ops x]))

(defmacro -equals [coerce-fn x y]
  `(.equals (~coerce-fn ~x) (~coerce-fn ~y)))

(deftype ByteOps []
  Ops
  ; the only time this gets called is when comparing two Bytes, so no need to coerce to Byte
  (ops-equals [_ x y] (.equals x y)))

(deftype ShortOps []
  Ops
  (ops-equals [_ x y] (-equals ->short x y))
  )

(deftype IntegerOps []
  Ops
  (ops-add                       [_ x y] (NumberOps/intAdd (->int x) (->int y)))
  (ops-bit-and                   [_ x y] (NumberOps/intBitAnd (->int x) (->int y)))
  (ops-bit-count                 [_ i]   (Integer/bitCount i))
  (ops-bit-or                    [_ x y] (NumberOps/intBitOr (->int x) (->int y)))
  (ops-bit-xor                   [_ x y] (NumberOps/intBitXor (->int x) (->int y)))
  (ops-bit-shift-left            [_ x y] (NumberOps/intBitShiftLeft (->int x) (->int y)))
  (ops-bit-unsigned-shift-right  [_ x y] (NumberOps/intBitUnsignedShiftRight (->int x) (->int y)))
  (ops-decrement                 [_ i]   (NumberOps/intDecrement (->int i)))
  (ops-equals                    [_ x y] (-equals ->int x y))
  (ops-increment                 [_ i]   (NumberOps/intIncrement (->int i)))
  (ops-multiply                  [_ x y] (NumberOps/intMultiply (->int x) (->int y)))
  (ops-subtract                  [_ x y] (NumberOps/intSubtract (->int x) (->int y))))

(deftype LongOps []
  Ops
  (ops-equals                    [_ x y] (-equals ->long x y))
  ;(ops-bit-and                   [_ x y] (NumberOps/longBitAnd (->long x) (->long y)))
  ;(ops-bit-or                    [_ x y] (NumberOps/longBitOr (->long x) (->long y)))
  (ops-bit-xor                   [_ x y] (NumberOps/longBitXor (->long x) (->long y)))
  ;(ops-bit-shift-left            [_ x y] (NumberOps/longBitShiftLeft (->long x) (->long y)))
  (ops-bit-unsigned-shift-right  [_ x y] (NumberOps/longBitUnsignedShiftRight (->long x) (->long y)))
  )

(deftype FloatOps []
  Ops
  (ops-equals [this x y] (-equals ->float x y))
  )

(deftype DoubleOps []
  Ops
  (ops-equals [_ x y] (-equals ->double x y))
  )

(deftype RatioOps []
  Ops
  (ops-equals [_ x y] (-equals ->ratio x y))
  )

(deftype BigIntegerOps []
  Ops
  (ops-equals [_ x y] (-equals ->bigint x y))
  )

(def INTEGER-ZERO (Integer. "0"))

(deftype BigDecimalOps []
  Ops
  (ops-equals [this x y]
    (.equals (.compareTo (->bigdec x) (->bigdec y))
             INTEGER-ZERO))
  (ops-zero?  [this x]
    (.equals (.signum x) INTEGER-ZERO)))

(def BYTE-OPS (ByteOps.))
(def SHORT-OPS (ShortOps.))
(def INTEGER-OPS (IntegerOps.))
(def LONG-OPS (LongOps.))
(def FLOAT-OPS (FloatOps.))
(def DOUBLE-OPS (DoubleOps.))
(def RATIO-OPS (RatioOps.))
(def BIGINTEGER-OPS (BigIntegerOps.))
(def BIGDECIMAL-OPS (BigDecimalOps.))

(defprotocol MakeOps
  (make-ops [this]))

(extend-protocol MakeOps
  Integer
  (make-ops [this] INTEGER-OPS)

  Long
  (make-ops [this] LONG-OPS)

  BigDecimal
  (make-ops [this] BIGDECIMAL-OPS))

(defmulti no-overflow-ops (fn [t1 t2] [t1 t2]))

(defmethod no-overflow-ops [Byte Byte]          [ops1 ops2] BYTE-OPS)
(defmethod no-overflow-ops [Byte Short]         [ops1 ops2] SHORT-OPS)
(defmethod no-overflow-ops [Byte Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Byte AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Byte Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Byte AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Byte Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Byte Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Byte BigInt]        [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Byte BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Byte BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Byte Ratio]         [ops1 opts] RATIO-OPS)

(defmethod no-overflow-ops [Short Byte]          [ops1 ops2] SHORT-OPS)
(defmethod no-overflow-ops [Short Short]         [ops1 ops2] SHORT-OPS)
(defmethod no-overflow-ops [Short Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Short AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Short Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Short AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Short Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Short Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Short BigInt]        [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Short BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Short BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Short Ratio]         [ops1 ops2] RATIO-OPS)

(defmethod no-overflow-ops [Integer Byte]          [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer Short]         [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Integer AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Integer Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Integer Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Integer BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Integer BigInt]        [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Integer BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Integer Ratio]         [ops1 ops2] INTEGER-OPS)

(defmethod no-overflow-ops [AtomicInteger Byte]          [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger Short]         [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicInteger AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicInteger Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [AtomicInteger Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [AtomicInteger BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger BigInt]        [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [AtomicInteger Ratio]         [ops1 ops2] INTEGER-OPS)

(defmethod no-overflow-ops [Long Byte]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long Short]         [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long Integer]       [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long AtomicInteger] [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Long Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Long BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Long BigInt]        [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Long BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Long Ratio]         [ops1 ops2] LONG-OPS)

(defmethod no-overflow-ops [AtomicLong Byte]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong Short]         [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong Integer]       [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong AtomicInteger] [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [AtomicLong Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [AtomicLong BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [AtomicLong BigInt]        [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [AtomicLong BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [AtomicLong Ratio]         [ops1 ops2] LONG-OPS)

(defmethod no-overflow-ops [Float Byte]          [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float Short]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float Integer]       [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float AtomicInteger] [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float Long]          [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float AtomicLong]    [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Float Ratio]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float BigInt]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Double Byte]          [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Short]         [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Integer]       [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double AtomicInteger] [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Long]          [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double AtomicLong]    [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Float]         [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Ratio]         [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Double BigInt]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Double BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Ratio Byte]          [ops1 ops2] RATIO-OPS)
(defmethod no-overflow-ops [Ratio Short]         [ops1 ops2] RATIO-OPS)
(defmethod no-overflow-ops [Ratio Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Ratio AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Ratio Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Ratio AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Ratio Float]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Ratio Double]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Ratio BigInt]        [ops2 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Ratio BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Ratio BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Ratio Ratio]         [ops1 ops2] RATIO-OPS)

(defmethod no-overflow-ops [BigInteger Byte]          [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger Short]         [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger Integer]       [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger AtomicInteger] [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger Long]          [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger AtomicLong]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger Float]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger Double]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger BigInt]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger Ratio]         [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [BigInt Byte]          [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInt Short]         [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInt Integer]       [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInt AtomicInteger] [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInt Long]          [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInt AtomicLong]    [ops1 ops2] BIGINTEGER-OPS)
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
  (-bit-and                  [this other])
  (-bit-count                [this])
  (-bit-or                   [this other])
  (-bit-xor                  [this other])
  (-bit-shift-left           [this shift])
  (-bit-unsigned-shift-right [this shift]))

(defn- long-hash-code [lpart]
  (unsafe-cast-int
    (-bit-xor
      lpart
      (-bit-unsigned-shift-right lpart 32))))

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
  (-hash [this]
    (-bit-xor
      (.hashCode (numerator this))
      (.hashCode (denominator this))))

  )

(defn- equivalent? [this other]
  (-> (no-overflow-ops (type this) (type other))
    (ops-equals this other)))

(defprotocol MathOperations
  (-add       [x y])
  (-subtract  [x y])
  (-multiply  [x y])
  (-increment [x])
  (-decrement [x]))

(extend-type Ratio
  IEquivalence
  (-equivalent? [this other]
    (if (or (instance? Number other) (instance? Ratio other))
      (equivalent? this other)
      false))

  (-equal? [this other]
    (if (instance? Ratio other)
      (-equivalent? this other)
      false))

  BitOperations
  (-bit-and [this other]
    (-> (no-overflow-ops (type this) (type other))
      (ops-bit-and this other)))

  (-bit-count [i]
    (ops-bit-count (make-ops i) i))

  (-bit-or [this other]
    (-> (no-overflow-ops (type this) (type other))
      (ops-bit-or this other)))

  (-bit-xor [this other]
    (-> (no-overflow-ops (type this) (type other))
      (ops-bit-xor this other)))

  (-bit-shift-left [this shift]
    (-> (no-overflow-ops (type this) (type shift))
      (ops-bit-shift-left this shift)))

  (-bit-unsigned-shift-right [this shift]
    (-> (no-overflow-ops (type this) (type shift))
      (ops-bit-unsigned-shift-right this shift)))

  MathOperations
  (-add [x y]
    (-> (no-overflow-ops (type x) (type y))
      (ops-add x y)))

  (-subtract [x y]
    (-> (no-overflow-ops (type x) (type y))
      (ops-subtract x y)))

  (-multiply [x y]
    (-> (no-overflow-ops (type x) (type y))
      (ops-multiply x y)))

  (-increment [i]
    (ops-increment (make-ops i) i))

  (-decrement [i]
    (ops-decrement (make-ops i) i))

  )

(extend-type Number
  IEquivalence
  (-equivalent? [this other]
    (if (or (instance? Number other) (instance? Ratio other))
      (equivalent? this other)
      false))

  (-equal? [this other]
    (if (instance? Number other)
      (if (.equals (category this)
                   (category other))
        (-equivalent? this other)
        false)
      false))

  BitOperations
  (-bit-and [this other]
    (-> (no-overflow-ops (type this) (type other))
      (ops-bit-and this other)))

  (-bit-count [i]
    (ops-bit-count (make-ops i) i))

  (-bit-or [this other]
    (-> (no-overflow-ops (type this) (type other))
      (ops-bit-or this other)))

  (-bit-xor [this other]
    (-> (no-overflow-ops (type this) (type other))
      (ops-bit-xor this other)))

  (-bit-shift-left [this shift]
    (-> (no-overflow-ops (type this) (type shift))
      (ops-bit-shift-left this shift)))

  (-bit-unsigned-shift-right [this shift]
    (-> (no-overflow-ops (type this) (type shift))
      (ops-bit-unsigned-shift-right this shift)))

  MathOperations
  (-add [x y]
    (-> (no-overflow-ops (type x) (type y))
      (ops-add x y)))

  (-subtract [x y]
    (-> (no-overflow-ops (type x) (type y))
      (ops-subtract x y)))

  (-multiply [x y]
    (-> (no-overflow-ops (type x) (type y))
      (ops-multiply x y)))

  (-increment [i]
    (ops-increment (make-ops i) i))

  (-decrement [i]
    (ops-decrement (make-ops i) i))

  )
