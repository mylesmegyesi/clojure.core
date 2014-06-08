; Temporary namespace to help us transition to the seq methods

(ns clojure.lang.platform.seqable
  (:refer-clojure :only [extend-type fn defn deftype declare when-let])
  (:require [clojure.lang.protocols :refer [ICounted ISeqable ISeq]]))

(declare old-seq->new-seq)

(clojure.lang.aseq/defseq OldSeqAdapter [old]
  ICounted
  (-count [this] (clojure.core/count old))

  ISeq
  (-first [this] (clojure.core/first old))

  (-next [this] (clojure.core/next old))

  (-more [this] (clojure.core/rest old)))

(defn old-seq->new-seq [old]
  (when-let [old (clojure.core/seq old)]
    (OldSeqAdapter. old)))

(extend-type clojure.lang.ISeq
  ISeqable
  (-seq [this] (old-seq->new-seq this)))
