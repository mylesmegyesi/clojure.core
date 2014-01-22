(ns clojure.lang.platform.seqable
  (:refer-clojure :only [extend-type fn defn deftype declare when])
  (:require [clojure.lang.icounted :refer [ICounted]]
            [clojure.lang.iseqable :refer [ISeqable]]
            [clojure.lang.iseq     :refer [ISeq]]
            [clojure.lang.seq      :refer [first next]]))

(declare old-seq->new-seq)

(deftype OldSeqAdapter [count first-entry next-seq]
  ICounted
  (-count [this] count)

  ISeq
  (-first [this] first-entry)

  (-next [this]
    (old-seq->new-seq next-seq)))

(defn old-seq->new-seq [old]
  (when old
    (OldSeqAdapter. (clojure.core/count old)
                    (clojure.core/first old)
                    (clojure.core/next old))))

(extend-type clojure.lang.ISeq
  ISeqable
  (-seq [this] (old-seq->new-seq this)))
