(ns clojure.support.test-seq
  (:refer-clojure :only [conj count defn deftype first loop])
  (:require [clojure.lang.protocols :refer [ICounted ISeq ISeqable ISequential]]
            [clojure.next           :refer :all :exclude [first count conj]]))

(deftype TestSeq [-list]
  ICounted
  (-count [this]
    (count -list))

  ISeqable
  (-seq [this]
    this)

  ISequential
  ISeq
  (-first [this]
    (first -list))

  (-next [this]
    (if (next -list)
      (TestSeq. (next -list))
      nil))

  (-more [this]
    (TestSeq. (rest -list))))

(deftype TestSeqable [-list]
  ISequential
  ISeqable
  (-seq [this]
    (if (zero? (count -list))
      nil
      (TestSeq. -list))))

(defn test-seqable [-list]
  (TestSeqable. -list))

(defn test-seq [-list]
  (TestSeq. -list))
