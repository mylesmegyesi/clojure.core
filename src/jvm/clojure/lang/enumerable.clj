(ns clojure.lang.enumerable
  (:refer-clojure :only [defmacro deftype let loop reset! defn update-in fn cons list satisfies? when])
  (:require [clojure.next :refer :all :exclude [cons]]
            [clojure.lang.numbers   :refer [unsafe-cast-int]]
            [clojure.lang.protocols :refer [IPersistentVector -array-for]]))

(def base-enumerator Iterable)
(def base-enumerable java.util.Iterator)

(defn enumerable? [x]
  (if x
    (or (instance? java.util.List x) (satisfies? IPersistentVector x))
    false))

(defn enumerable-equals? [^java.util.Collection x ^java.util.Collection y]
  (if (or (not= (.size x) (.size y)) (not= (.hashCode x) (.hashCode y)))
    false
    (loop [iter-x (.iterator x)
           iter-y (.iterator y)]
      (if (.hasNext iter-x)
        (if (not= (.next iter-x) (.next iter-y))
          false
          (recur iter-x iter-y))
        true))))

(deftype SeqIterator [^:unsynchronized-mutable -current-seq]
  java.util.Iterator
  (hasNext [this]
    (if -current-seq true false))

  (next [this]
    (if -current-seq
      (let [first-item (first -current-seq)]
        (set! -current-seq (next -current-seq))
        first-item)
      (throw (java.util.NoSuchElementException. ""))))

  (remove [this]
    (throw (UnsupportedOperationException. ""))))

(deftype RangedIterator [^:unsynchronized-mutable -i
                         -end
                         ^:unsynchronized-mutable -base
                         ^:unsynchronized-mutable -arr
                         -vec]

  java.util.Iterator
  (hasNext [this]
    (< -i -end))

  (next [this]
    (when (= (- -i -base) 32)
      (set! -arr (-array-for -vec -i))
      (set! -base (+ -base 32)))
    (let [ret (aget -arr (bit-and (unsafe-cast-int -i) (unsafe-cast-int 0x01f)))]
      (set! -i (inc -i))
      ret))

  (remove [this]
    (throw (UnsupportedOperationException.))))

(defn new-ranged-iterator [v start end]
  (let [base (- start (mod start 32))
        arr (if (< start (count v)) (-array-for v start) nil)]
    (RangedIterator. start end base arr v)))

(defn new-seq-iterator [-seq]
  (SeqIterator. -seq))

(defn has-more-elements? [^java.util.Enumeration iter]
  (.hasMoreElements iter))

(defn get-next [^java.util.Enumeration iter]
  (.nextElement iter))

(defmacro enumerable-method [bindings & body]
  `(iterator ~bindings ~@body))

