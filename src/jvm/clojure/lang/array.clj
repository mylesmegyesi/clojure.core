(ns clojure.lang.array
  (:refer-clojure :only [cond cons defmacro defn dotimes let loop when while])
  (:require [clojure.next :refer :all :exclude [cons]])
  (:import [java.lang.reflect Array]
           [java.util ArrayList Collection Map]))

(def object-array-type (Class/forName "[Ljava.lang.Object;"))

(defn get-array-type [arr]
  (.getComponentType (.getClass arr)))

(defn make-array
  ([^Class c size]
    (Array/newInstance c size))
  ([^Class c d ds]
    (let [dims (cons d ds)
          ^"[I" dimarray (make-array Integer/TYPE (count dims))]
      (dotimes [i (Array/getLength dimarray)]
        (Array/setInt dimarray i (int (clojure.core/nth dims i))))
      (Array/newInstance c dimarray))))

(def EMPTY-ARRAY (make-array Object 0))

(defmacro array-set! [arr idx v]
  `(Array/set ~arr ~idx ~v))

(defmacro array-get [arr idx]
  `(Array/get ~arr ~idx))

(defmacro array-copy [src src-pos dest dest-pos length]
  `(System/arraycopy ~src ~src-pos ~dest ~dest-pos ~length))

(defmacro array-length [arr]
  `(Array/getLength ~arr))

(defmacro array-clone [arr]
  `(let [arr# ~arr
         size# (array-length arr#)
         new-arr# (make-array (get-array-type arr#) size#)]
     (array-copy arr# 0 new-arr# 0 size#)
     new-arr#))

(defmacro array-set-byte! [arr idx v]
  `(Array/setByte ~arr ~idx ~v))

(defmacro array-set-short! [arr idx v]
  `(Array/setShort ~arr ~idx ~v))

(defmacro array-set-int! [arr idx v]
  `(Array/setInt ~arr ~idx ~v))

(defmacro array-set-long! [arr idx v]
  `(Array/setLong ~arr ~idx ~v))

(defmacro array-set-float! [arr idx v]
  `(Array/setFloat ~arr ~idx ~v))

(defmacro array-set-double! [arr idx v]
  `(Array/setDouble ~arr ~idx ~v))

(defmacro to-array [coll]
  `(let [coll# ~coll]
     (cond
      (instance? object-array-type coll#)
        coll#
      (instance? Collection coll#)
        (.toArray coll#)
      (instance? Iterable coll#)
        (let [ret# (ArrayList.)
              iter# (.iterator coll#)]
          (while (.hasNext iter#)
            (.add ret# (.next iter#)))
          (.toArray ret#))
      (instance? Map coll#)
        (.toArray (.entrySet coll#))
      (instance? String coll#)
        (let [chrs# (.toCharArray coll#)
              len# (array-length chrs#)
              ret# (make-array Object len#)]
          (loop [i# 0]
            (when (< i# len#)
              (do (array-set! ret# i# (array-get chrs# i#)) (recur (inc i#)))))
          ret#)
      (.isArray (.getClass coll#))
        (let [s# (seq coll#)
              len# (count s#)
              ret# (make-array Object len#)]
          (loop [i# 0]
            (when (< i# len#)
              (do (array-set! ret# i# (nth s# i#)) (recur (inc i#)))))
          ret#)
      :else
        (throw (RuntimeException. (str "Unable to convert: " (.getClass coll#) " to Object[]"))))))

