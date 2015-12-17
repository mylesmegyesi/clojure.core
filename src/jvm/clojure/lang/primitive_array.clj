(ns clojure.lang.primitive-array
  (:refer-clojure :only [defmacro defn])
  (:require [clojure.next           :refer :all]
            [clojure.lang.protocols :refer [ISeq]])
  (:import  [clojure.lang.platform PrimitiveArray]))

(def platform-boolean        Boolean)
(def platform-native-boolean Boolean/TYPE)
(def platform-char           Character)

(defmacro to-booleans [arr]
  `(PrimitiveArray/castToBooleans ~arr))

(defmacro to-bytes [arr]
  `(PrimitiveArray/castToBytes ~arr))

(defmacro to-chars [arr]
  `(PrimitiveArray/castToChars ~arr))

(defmacro to-shorts [arr]
  `(PrimitiveArray/castToShorts ~arr))

(defmacro to-floats [arr]
  `(PrimitiveArray/castToFloats ~arr))

(defmacro to-doubles [arr]
  `(PrimitiveArray/castToDoubles ~arr))

(defmacro to-ints [arr]
  `(PrimitiveArray/castToInts ~arr))

(defmacro to-longs [arr]
  `(PrimitiveArray/castToLongs ~arr))

(defn byte-array-for-size [size]
  (PrimitiveArray/byteArrayForSize ^Number size))

(defn short-array-for-size [size]
  (PrimitiveArray/shortArrayForSize ^Number size))

(defn int-array-for-size [size]
  (PrimitiveArray/intArrayForSize ^Number size))

(defn long-array-for-size [size]
  (PrimitiveArray/longArrayForSize ^Number size))

(defn float-array-for-size [size]
  (PrimitiveArray/floatArrayForSize ^Number size))

(defn double-array-for-size [size]
  (PrimitiveArray/doubleArrayForSize ^Number size))

(defn boolean-array-for-size [size]
  (PrimitiveArray/booleanArrayForSize ^Number size))

(defn char-array-for-size [size]
  (PrimitiveArray/charArrayForSize ^Number size))

