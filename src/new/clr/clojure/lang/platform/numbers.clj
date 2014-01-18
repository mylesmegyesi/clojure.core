(ns clojure.lang.platform.numbers
  (:refer-clojure :only [defmacro defprotocol deftype defmulti defmethod defn defn- extend-protocol extend-type fn -> contains? cond let])
  (:require [clojure.lang.iequivalence     :refer [IEquivalence -equivalent?]]
            [clojure.lang.ihash            :refer [IHash]]
            [clojure.lang.platform.object  :refer [type instance? expand-methods]]))

(defn ->char [thing]
  (Convert/ToSByte thing))

(defn ->sbyte [thing]
  (Convert/ToSByte thing))

(defn ->byte [thing]
  (Convert/ToByte thing))

(defn ->int16 [thing]
  (Convert/ToInt16 thing))

(defn ->int32 [thing]
  (Convert/ToInt32 thing))

(defn ->int64 [thing]
  (Convert/ToInt64 thing))

(defprotocol Ops
  (ops-equals [ops x y])
  (ops-add    [ops x y])
  (zero?      [ops x]))

(deftype CharOps []
  Ops
  (ops-equals [_ x y] (.Equals (->char x) (->char y))))

(deftype SByteOps []
  Ops
  (ops-equals [_ x y] (.Equals (->sbyte x) (->sbyte y))))

(deftype ByteOps []
  Ops
  (ops-equals [_ x y]
    (.Equals (->byte x) (->byte y))))

(deftype Int16Ops []
  Ops
  (ops-equals [_ x y]
    (.Equals (->int16 x) (->int16 y))))

(deftype Int32Ops []
  Ops
  (ops-equals [_ x y]
    (.Equals (->int32 x) (->int32 y))))

(deftype Int64Ops []
  Ops
  (ops-equals [_ x y]
    (.Equals (->int64 x) (->int64 y))))

(def CHAR-OPS  (CharOps.))
(def SBYTE-OPS (SByteOps.))
(def BYTE-OPS  (ByteOps.))
(def INT16-OPS (Int16Ops.))
(def INT32-OPS (Int32Ops.))
(def INT64-OPS (Int64Ops.))

(defmulti no-overflow-ops (fn [t1 t2] [t1 t2]))

(defmethod no-overflow-ops [Char Char]  [_ _] CHAR-OPS)
(defmethod no-overflow-ops [Char SByte] [_ _] SBYTE-OPS)
(defmethod no-overflow-ops [Char Byte]  [_ _] BYTE-OPS)
(defmethod no-overflow-ops [Char Int16] [_ _] INT16-OPS)
(defmethod no-overflow-ops [Char Int32] [_ _] INT32-OPS)
(defmethod no-overflow-ops [Char Int64] [_ _] INT64-OPS)

(defmethod no-overflow-ops [SByte Char]  [_ _] SBYTE-OPS)
(defmethod no-overflow-ops [SByte SByte] [_ _] SBYTE-OPS)
(defmethod no-overflow-ops [SByte Byte]  [_ _] BYTE-OPS)
(defmethod no-overflow-ops [SByte Int16] [_ _] INT16-OPS)
(defmethod no-overflow-ops [SByte Int32] [_ _] INT32-OPS)
(defmethod no-overflow-ops [SByte Int64] [_ _] INT64-OPS)

(defmethod no-overflow-ops [Byte Char]  [_ _] BYTE-OPS)
(defmethod no-overflow-ops [Byte SByte] [_ _] BYTE-OPS)
(defmethod no-overflow-ops [Byte Byte]  [_ _] BYTE-OPS)
(defmethod no-overflow-ops [Byte Int16] [_ _] INT16-OPS)
(defmethod no-overflow-ops [Byte Int32] [_ _] INT32-OPS)
(defmethod no-overflow-ops [Byte Int64] [_ _] INT64-OPS)

(defmethod no-overflow-ops [Int16 Char]  [_ _] INT16-OPS)
(defmethod no-overflow-ops [Int16 SByte] [_ _] INT16-OPS)
(defmethod no-overflow-ops [Int16 Byte]  [_ _] INT16-OPS)
(defmethod no-overflow-ops [Int16 Int16] [_ _] INT16-OPS)
(defmethod no-overflow-ops [Int16 Int32] [_ _] INT32-OPS)
(defmethod no-overflow-ops [Int16 Int64] [_ _] INT64-OPS)

(defmethod no-overflow-ops [Int32 Char]  [_ _] INT32-OPS)
(defmethod no-overflow-ops [Int32 SByte] [_ _] INT32-OPS)
(defmethod no-overflow-ops [Int32 Byte]  [_ _] INT32-OPS)
(defmethod no-overflow-ops [Int32 Int16] [_ _] INT32-OPS)
(defmethod no-overflow-ops [Int32 Int32] [_ _] INT32-OPS)
(defmethod no-overflow-ops [Int32 Int64] [_ _] INT64-OPS)

(defmethod no-overflow-ops [Int64 Char]  [_ _] INT64-OPS)
(defmethod no-overflow-ops [Int64 SByte] [_ _] INT64-OPS)
(defmethod no-overflow-ops [Int64 Byte]  [_ _] INT64-OPS)
(defmethod no-overflow-ops [Int64 Int16] [_ _] INT64-OPS)
(defmethod no-overflow-ops [Int64 Int32] [_ _] INT64-OPS)
(defmethod no-overflow-ops [Int64 Int64] [_ _] INT64-OPS)

(defn char-type? [type]
  (.IsAssignableFrom Char type))

(defn sbyte-type? [type]
  (.IsAssignableFrom SByte type))

(defn byte-type? [type]
  (.IsAssignableFrom Byte type))

(defn int16-type? [type]
  (.IsAssignableFrom Int16 type))

(defn int32-type? [type]
  (.IsAssignableFrom Int32 type))

(defn int64-type? [type]
  (.IsAssignableFrom Int64 type))

(defn category [num]
  (let [num-type (.GetType num)]
    (cond
      (char-type? num-type)  :integer
      (sbyte-type? num-type) :integer
      (byte-type? num-type)  :integer
      (int16-type? num-type) :integer
      (int32-type? num-type) :integer
      (int64-type? num-type) :integer
      )))

(def all-num-types #{Char SByte Byte Int16 Int32 Int64})

(defn number? [thing]
  (contains? all-num-types (.GetType thing)))

(defmacro unchecked-num-equiv? [this other]
  `(-> (no-overflow-ops (type ~this) (type ~other))
     (ops-equals ~this ~other)))

(defmacro num-equivalent? [this other]
  `(if (number? ~other)
     (unchecked-num-equiv? ~this ~other)
     false))

(defmacro num-equal? [this other]
  `(if (number? ~other)
     (if (.Equals (category ~this)
                  (category ~other))
       (unchecked-num-equiv? ~this ~other)
       false)
     false))
