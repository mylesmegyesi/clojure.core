(ns clojure.lang.platform.equivalence
  (:refer-clojure :only [defmacro defprotocol deftype extend-protocol extend-type defmulti defmethod fn defn list -> update-in cons])
  (:require [clojure.lang.iequivalence    :refer [IEquivalence]]
            [clojure.lang.platform.object :refer [type instance?]])
  (:import [java.lang Number Short Byte Integer Long Float Double]
           [java.math BigInteger BigDecimal]
           [java.util.concurrent.atomic AtomicInteger AtomicLong]))

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
  (->short [this] this)
  (->int   [this] (.intValue this))
  (->long  [this] (.longValue this))
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
  (->float [this] this)
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
  (ops-equals [ops x y]))

(defmacro -equals [coerce-fn x y]
  `(.equals (~coerce-fn ~x) (~coerce-fn ~y)))

(deftype ByteOps []
  Ops
  (ops-equals [ops x y]
    (-equals ->byte x y)))

(deftype ShortOps []
  Ops
  (ops-equals [ops x y]
    (-equals ->short x y)))

(deftype IntegerOps []
  Ops
  (ops-equals [ops x y]
    (-equals ->int x y)))

(deftype LongOps []
  Ops
  (ops-equals [this x y]
    (-equals ->long x y)))

(deftype FloatOps []
  Ops
  (ops-equals [this x y]
    (-equals ->float x y)))

(deftype DoubleOps []
  Ops
  (ops-equals [this x y]
    (-equals ->double x y)))

(deftype BigIntegerOps []
  Ops
  (ops-equals [this x y]
    (-equals ->bigint x y)))

(deftype BigDecimalOps []
  Ops
  (ops-equals [this x y]
    (.equals (.compareTo (->bigdec x) (->bigdec y))
             (Integer. "0"))))

(defprotocol MakeOps
  (make-ops [this]))

(def BYTE-OPS (ByteOps.))
(def SHORT-OPS (ShortOps.))
(def INTEGER-OPS (IntegerOps.))
(def LONG-OPS (LongOps.))
(def FLOAT-OPS (FloatOps.))
(def DOUBLE-OPS (DoubleOps.))
(def BIGINTEGER-OPS (BigIntegerOps.))
(def BIGDECIMAL-OPS (BigDecimalOps.))

(extend-protocol MakeOps
  Byte
  (make-ops [this] BYTE-OPS)

  Short
  (make-ops [this] SHORT-OPS)

  Integer
  (make-ops [this] INTEGER-OPS)

  AtomicInteger
  (make-ops [this] INTEGER-OPS)

  Long
  (make-ops [this] LONG-OPS)

  AtomicLong
  (make-ops [this] LONG-OPS)

  Float
  (make-ops [this] FLOAT-OPS)

  Double
  (make-ops [this] DOUBLE-OPS)

  BigInteger
  (make-ops [this] BIGINTEGER-OPS)

  BigDecimal
  (make-ops [this] BIGDECIMAL-OPS))

(defmulti ops-combine (fn [ops1 ops2] [(type ops1) (type ops2)]))

(defmethod ops-combine [ByteOps ByteOps]       [ops1 ops2] ops2)
(defmethod ops-combine [ByteOps ShortOps]      [ops1 ops2] ops2)
(defmethod ops-combine [ByteOps IntegerOps]    [ops1 ops2] ops2)
(defmethod ops-combine [ByteOps LongOps]       [ops1 ops2] ops2)
(defmethod ops-combine [ByteOps FloatOps]      [ops1 ops2] ops2)
(defmethod ops-combine [ByteOps DoubleOps]     [ops1 ops2] ops2)
(defmethod ops-combine [ByteOps BigIntegerOps] [ops1 ops2] ops2)
(defmethod ops-combine [ByteOps BigDecimalOps] [ops1 ops2] ops2)

(defmethod ops-combine [ShortOps ByteOps]       [ops1 ops2] ops1)
(defmethod ops-combine [ShortOps ShortOps]      [ops1 ops2] ops2)
(defmethod ops-combine [ShortOps IntegerOps]    [ops1 ops2] ops2)
(defmethod ops-combine [ShortOps LongOps]       [ops1 ops2] ops2)
(defmethod ops-combine [ShortOps FloatOps]      [ops1 ops2] ops2)
(defmethod ops-combine [ShortOps DoubleOps]     [ops1 ops2] ops2)
(defmethod ops-combine [ShortOps BigIntegerOps] [ops1 ops2] ops2)
(defmethod ops-combine [ShortOps BigDecimalOps] [ops1 ops2] ops2)

(defmethod ops-combine [IntegerOps ByteOps]       [ops1 ops2] ops1)
(defmethod ops-combine [IntegerOps ShortOps]      [ops1 ops2] ops1)
(defmethod ops-combine [IntegerOps IntegerOps]    [ops1 ops2] ops2)
(defmethod ops-combine [IntegerOps LongOps]       [ops1 ops2] ops2)
(defmethod ops-combine [IntegerOps FloatOps]      [ops1 ops2] ops2)
(defmethod ops-combine [IntegerOps DoubleOps]     [ops1 ops2] ops2)
(defmethod ops-combine [IntegerOps BigIntegerOps] [ops1 ops2] ops2)
(defmethod ops-combine [IntegerOps BigDecimalOps] [ops1 ops2] ops2)

(defmethod ops-combine [LongOps ByteOps]       [ops1 ops2] ops1)
(defmethod ops-combine [LongOps ShortOps]      [ops1 ops2] ops1)
(defmethod ops-combine [LongOps IntegerOps]    [ops1 ops2] ops1)
(defmethod ops-combine [LongOps LongOps]       [ops1 ops2] ops2)
(defmethod ops-combine [LongOps FloatOps]      [ops1 ops2] ops2)
(defmethod ops-combine [LongOps DoubleOps]     [ops1 ops2] ops2)
(defmethod ops-combine [LongOps BigIntegerOps] [ops1 ops2] ops2)
(defmethod ops-combine [LongOps BigDecimalOps] [ops1 ops2] ops2)

(defmethod ops-combine [FloatOps ByteOps]       [ops1 ops2] ops1)
(defmethod ops-combine [FloatOps ShortOps]      [ops1 ops2] ops1)
(defmethod ops-combine [FloatOps IntegerOps]    [ops1 ops2] ops1)
(defmethod ops-combine [FloatOps LongOps]       [ops1 ops2] ops1)
(defmethod ops-combine [FloatOps FloatOps]      [ops1 ops2] ops2)
(defmethod ops-combine [FloatOps DoubleOps]     [ops1 ops2] ops2)
(defmethod ops-combine [FloatOps BigIntegerOps] [ops1 ops2] BIGDECIMAL-OPS)
(defmethod ops-combine [FloatOps BigDecimalOps] [ops1 ops2] ops2)

(defmethod ops-combine [DoubleOps ByteOps]       [ops1 ops2] ops1)
(defmethod ops-combine [DoubleOps ShortOps]      [ops1 ops2] ops1)
(defmethod ops-combine [DoubleOps IntegerOps]    [ops1 ops2] ops1)
(defmethod ops-combine [DoubleOps LongOps]       [ops1 ops2] ops1)
(defmethod ops-combine [DoubleOps FloatOps]      [ops1 ops2] ops1)
(defmethod ops-combine [DoubleOps DoubleOps]     [ops1 ops2] ops2)
(defmethod ops-combine [DoubleOps BigIntegerOps] [ops1 ops2] BIGDECIMAL-OPS)
(defmethod ops-combine [DoubleOps BigDecimalOps] [ops1 ops2] ops2)

(defmethod ops-combine [BigIntegerOps ByteOps]       [ops1 ops2] ops1)
(defmethod ops-combine [BigIntegerOps ShortOps]      [ops1 ops2] ops1)
(defmethod ops-combine [BigIntegerOps IntegerOps]    [ops1 ops2] ops1)
(defmethod ops-combine [BigIntegerOps LongOps]       [ops1 ops2] ops1)
(defmethod ops-combine [BigIntegerOps FloatOps]      [ops1 ops2] BIGDECIMAL-OPS)
(defmethod ops-combine [BigIntegerOps DoubleOps]     [ops1 ops2] BIGDECIMAL-OPS)
(defmethod ops-combine [BigIntegerOps BigIntegerOps] [ops1 ops2] BIGDECIMAL-OPS)
(defmethod ops-combine [BigIntegerOps BigDecimalOps] [ops1 ops2] ops2)

(defmethod ops-combine [BigDecimalOps ByteOps]       [ops1 ops2] ops1)
(defmethod ops-combine [BigDecimalOps ShortOps]      [ops1 ops2] ops1)
(defmethod ops-combine [BigDecimalOps IntegerOps]    [ops1 ops2] ops1)
(defmethod ops-combine [BigDecimalOps LongOps]       [ops1 ops2] ops1)
(defmethod ops-combine [BigDecimalOps FloatOps]      [ops1 ops2] ops1)
(defmethod ops-combine [BigDecimalOps DoubleOps]     [ops1 ops2] ops1)
(defmethod ops-combine [BigDecimalOps BigIntegerOps] [ops1 ops2] ops1)
(defmethod ops-combine [BigDecimalOps BigDecimalOps] [ops1 ops2] ops1)

(extend-protocol IEquivalence
  Object
  (-equivalent? [this other]
    (.equals this other))

  Number
  (-equivalent? [this other]
    (if (instance? Number other)
      (-> (ops-combine (make-ops this) (make-ops other))
        (ops-equals this other))
      false)))

(defn platform-equals-method [methods init-macro]
  (update-in methods
             ['Object]
             (fn [old]
               (cons
                 (list 'equals ['this 'other]
                       (list init-macro 'this 'other))
                 old))))
