(ns clojure.lang.persistent-map
  (:refer-clojure :only [deftype defn defn- declare empty? let when-let when loop])
  (:require [clojure.lang.aseq            :refer [defseq]]
            [clojure.lang.counted         :refer [count]]
            [clojure.lang.map-entry       :refer [key val]]
            [clojure.lang.protocols       :refer [ICounted ISeq -assoc -dissoc]]
            [clojure.lang.seqable         :refer [seq]]
            [clojure.lang.seq             :refer [first next]]))

(defn assoc-seq [m kvs]
  (if kvs
    (let [n (next kvs)]
      (recur (-assoc m (first kvs) (first n)) (next n)))
    m))

(defn assoc
  ([m k v]
   (-assoc m k v))
  ([m k v & kvs]
   (assoc-seq (-assoc m k v) (seq kvs))))

(defn dissoc-seq [m ks]
  (if ks
    (recur (-dissoc m (first ks)) (next ks))
    m))

(defn dissoc
  ([m] m)
  ([m k] (-dissoc m k))
  ([m k & ks]
   (dissoc-seq (-dissoc m k) (seq ks))))

(declare new-key-seq)

(defseq KeySeq [seq]
  ICounted
  (-count [this]
    (count seq))

  ISeq
  (-first [this]
    (key (first seq)))

  (-next [this]
    (new-key-seq (next seq))))

(defn- new-key-seq [seq]
  (when seq
    (KeySeq. seq)))

(defn keys [m]
  (new-key-seq (seq m)))

(declare new-val-seq)

(defseq ValSeq [seq]
  ICounted
  (-count [this]
    (count seq))

  ISeq
  (-first [this]
    (val (first seq)))

  (-next [this]
    (new-val-seq (next seq))))

(defn- new-val-seq [seq]
  (when seq
    (ValSeq. seq)))

(defn vals [m]
  (new-val-seq (seq m)))
