(ns clojure.lang.persistent-map
  (:refer-clojure :only [defn declare if-let when])
  (:require [clojure.lang
              [aseq            :refer [defseq]]
              [deftype]
              [equivalence]
              [persistent-list :refer [EMPTY-LIST]]
              [protocols       :refer [ICounted ISeq ISeqable ISequential
                                       -assoc -dissoc]]]
            [clojure.next                 :refer :all]))

(declare new-key-seq)

(defseq KeySeq [-seq]
  ICounted
  (-count [this]
    (count -seq))

  ISeq
  (-first [this]
    (key (first -seq)))

  (-next [this]
    (new-key-seq (next -seq)))

  (-more [this]
    (if-let [m (next -seq)] (new-key-seq m) EMPTY-LIST)))

(defn new-key-seq [-seq]
  (when -seq
    (KeySeq. -seq)))

(declare new-val-seq)

(defseq ValSeq [-seq]
  ICounted
  (-count [this]
    (count -seq))

  ISeq
  (-first [this]
    (val (first -seq)))

  (-next [this]
    (new-val-seq (next -seq)))

  (-more [this]
    (if-let [m (next -seq)] (new-val-seq m) EMPTY-LIST)))

(defn new-val-seq [-seq]
  (when -seq
    (ValSeq. -seq)))
