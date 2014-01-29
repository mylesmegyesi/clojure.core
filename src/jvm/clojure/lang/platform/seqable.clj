(ns clojure.lang.platform.seqable
  (:refer-clojure :only [extend-type fn defn deftype declare when])
  (:require [clojure.lang.aseq     :refer [defseq]]
            [clojure.lang.icounted :refer [ICounted]]
            [clojure.lang.iseqable :refer [ISeqable]]
            [clojure.lang.iseq     :refer [ISeq]]))

(declare old-seq->new-seq)

(defseq OldSeqAdapter [old]
  ICounted
  (-count [this] (clojure.core/count old))

  ISeq
  (-first [this] (clojure.core/first old))

  (-next [this]
    (old-seq->new-seq (clojure.core/next old))))

(defn old-seq->new-seq [old]
  (when old
    (OldSeqAdapter. old)))

(extend-type clojure.lang.ISeq
  ISeqable
  (-seq [this] (old-seq->new-seq this)))
