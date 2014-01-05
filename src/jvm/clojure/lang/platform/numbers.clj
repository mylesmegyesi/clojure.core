(ns clojure.lang.platform.numbers
  (:refer-clojure :only [defmacro defprotocol deftype defmulti defmethod extend-protocol extend-type fn])
  (:import [java.lang Number Short Byte Integer Long Float Double]
           [java.math BigInteger BigDecimal]
           [java.util.concurrent.atomic AtomicInteger AtomicLong]
           [clojure.lang.platform NumberOps]))

(defprotocol NumberCoercions
  (->byte   [this])
  (->short  [this])
  (->int    [this])
  (->long   [this])
  (->float  [this])
  (->double [this])
  (->bigint [this])
  (->bigdec [this]))

(extend-type Byte
  NumberCoercions
  (->byte   [this] this)
  (->short  [this] (.shortValue this))
  (->int    [this] (.intValue this))
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. (.intValue this))))

(extend-type Short
  NumberCoercions
  (->short  [this] this)
  (->int    [this] (.intValue this))
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. (.intValue this))))

(extend-type Integer
  NumberCoercions
  (->int    [this] this)
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. this)))

(extend-type AtomicInteger
  NumberCoercions
  (->int    [this] (.intValue this))
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. (.intValue this))))

(extend-type Long
  NumberCoercions
  (->long   [this] this)
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
  (->bigint [this] (BigInteger. (.toString this)))
  (->bigdec [this] (BigDecimal. (.longValue this))))

(extend-type AtomicLong
  NumberCoercions
  (->long   [this] (.longValue this))
  (->float  [this] (.floatValue this))
  (->double [this] (.doubleValue this))
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
  (->bigint [this] this)
  (->bigdec [this] (BigDecimal. this)))

(extend-type BigDecimal
  NumberCoercions
  (->bigdec [this] this))

(defprotocol Ops
  (ops-equals [ops x y])
  (ops-add    [ops x y]))

(defmacro -equals [coerce-fn x y]
  `(.equals (~coerce-fn ~x) (~coerce-fn ~y)))

(deftype ByteOps []
  Ops
  ; the only time this gets called is when comparing two Bytes, so no need to coerce to Byte
  (ops-equals [_ x y] (.equals x y)))

(deftype ShortOps []
  Ops
  (ops-equals [_ x y] (-equals ->short x y))
  (ops-add    [_ x y] (NumberOps/shortAdd (->short x) (->short y)))
  )

(deftype IntegerOps []
  Ops
  (ops-equals [_ x y] (-equals ->int x y))
  (ops-add    [_ x y] (NumberOps/intAdd (->int x) (->int y)))
  )

(deftype LongOps []
  Ops
  (ops-equals [_ x y] (-equals ->long x y))
  (ops-add    [_ x y] (NumberOps/longAdd (->long x) (->long y)))
  )

(deftype FloatOps []
  Ops
  (ops-equals [this x y] (-equals ->float x y))
  )

(deftype DoubleOps []
  Ops
  (ops-equals [_ x y] (-equals ->double x y))
  )

(deftype BigIntegerOps []
  Ops
  (ops-equals [_ x y] (-equals ->bigint x y))
  (ops-add    [_ x y] (.add (->bigint x) (->bigint y)))
  )

(deftype BigDecimalOps []
  Ops
  (ops-equals [this x y]
    (.equals (.compareTo (->bigdec x) (->bigdec y))
             (Integer. "0"))))

(def BYTE-OPS (ByteOps.))
(def SHORT-OPS (ShortOps.))
(def INTEGER-OPS (IntegerOps.))
(def LONG-OPS (LongOps.))
(def FLOAT-OPS (FloatOps.))
(def DOUBLE-OPS (DoubleOps.))
(def BIGINTEGER-OPS (BigIntegerOps.))
(def BIGDECIMAL-OPS (BigDecimalOps.))

(defmulti no-overflow-ops (fn [t1 t2] [t1 t2]))

(defmethod no-overflow-ops [Byte Byte]          [ops1 ops2] BYTE-OPS)
(defmethod no-overflow-ops [Byte Short]         [ops1 ops2] SHORT-OPS)
(defmethod no-overflow-ops [Byte Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Byte AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Byte Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Byte AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Byte Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Byte Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Byte BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Byte BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Short Byte]          [ops1 ops2] SHORT-OPS)
(defmethod no-overflow-ops [Short Short]         [ops1 ops2] SHORT-OPS)
(defmethod no-overflow-ops [Short Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Short AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Short Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Short AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Short Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Short Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Short BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Short BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Integer Byte]          [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer Short]         [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [Integer Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Integer AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Integer Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Integer Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Integer BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Integer BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [AtomicInteger Byte]          [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger Short]         [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger Integer]       [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger AtomicInteger] [ops1 ops2] INTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicInteger AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicInteger Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [AtomicInteger Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [AtomicInteger BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [AtomicInteger BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Long Byte]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long Short]         [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long Integer]       [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long AtomicInteger] [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [Long Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Long Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Long BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [Long BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [AtomicLong Byte]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong Short]         [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong Integer]       [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong AtomicInteger] [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong Long]          [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong AtomicLong]    [ops1 ops2] LONG-OPS)
(defmethod no-overflow-ops [AtomicLong Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [AtomicLong Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [AtomicLong BigInteger]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [AtomicLong BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Float Byte]          [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float Short]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float Integer]       [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float AtomicInteger] [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float Long]          [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float AtomicLong]    [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float Float]         [ops1 ops2] FLOAT-OPS)
(defmethod no-overflow-ops [Float Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Float BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Float BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [Double Byte]          [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Short]         [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Integer]       [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double AtomicInteger] [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Long]          [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double AtomicLong]    [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Float]         [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double Double]        [ops1 ops2] DOUBLE-OPS)
(defmethod no-overflow-ops [Double BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [Double BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [BigInteger Byte]          [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger Short]         [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger Integer]       [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger AtomicInteger] [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger Long]          [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger AtomicLong]    [ops1 ops2] BIGINTEGER-OPS)
(defmethod no-overflow-ops [BigInteger Float]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger Double]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigInteger BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmethod no-overflow-ops [BigDecimal Byte]          [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Short]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Integer]       [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal AtomicInteger] [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Long]          [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal AtomicLong]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Float]         [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal Double]        [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal BigInteger]    [ops1 ops2] BIGDECIMAL-OPS)
(defmethod no-overflow-ops [BigDecimal BigDecimal]    [ops1 ops2] BIGDECIMAL-OPS)

(defmulti overflow-ops (fn [t1 t2] [t1 t2]))

(defmethod overflow-ops [Byte Byte]       [_ _] SHORT-OPS)
(defmethod overflow-ops [Byte Short]      [_ _] INTEGER-OPS)
(defmethod overflow-ops [Byte Integer]    [_ _] LONG-OPS)
(defmethod overflow-ops [Byte Long]       [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [Byte BigInteger] [_ _] BIGINTEGER-OPS)

(defmethod overflow-ops [Short Byte]       [_ _] INTEGER-OPS)
(defmethod overflow-ops [Short Short]      [_ _] INTEGER-OPS)
(defmethod overflow-ops [Short Integer]    [_ _] LONG-OPS)
(defmethod overflow-ops [Short Long]       [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [Short BigInteger] [_ _] BIGINTEGER-OPS)

(defmethod overflow-ops [Integer Byte]       [_ _] LONG-OPS)
(defmethod overflow-ops [Integer Short]      [_ _] LONG-OPS)
(defmethod overflow-ops [Integer Integer]    [_ _] LONG-OPS)
(defmethod overflow-ops [Integer Long]       [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [Integer BigInteger] [_ _] BIGINTEGER-OPS)

(defmethod overflow-ops [Long Byte]       [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [Long Short]      [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [Long Integer]    [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [Long Long]       [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [Long BigInteger] [_ _] BIGINTEGER-OPS)

(defmethod overflow-ops [BigInteger Byte]       [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [BigInteger Short]      [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [BigInteger Integer]    [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [BigInteger Long]       [_ _] BIGINTEGER-OPS)
(defmethod overflow-ops [BigInteger BigInteger] [_ _] BIGINTEGER-OPS)
