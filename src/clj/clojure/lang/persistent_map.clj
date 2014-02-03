(ns clojure.lang.persistent-map
  (:refer-clojure :only [deftype defn defn- declare empty? let when-let when loop])
  (:require [clojure.lang.aseq            :refer [defseq]]
            [clojure.lang.protocols       :refer [ICounted ISeq -assoc -dissoc]]
            [clojure.next                 :refer :all :exclude [empty?]]))

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
