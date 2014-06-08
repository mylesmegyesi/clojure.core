(ns clojure.support.test-seq
  (:refer-clojure :only [conj count defn deftype nth loop])
  (:require [clojure.lang.protocols :refer [ICounted ISeq ISeqable]]
            [clojure.next           :refer :all :exclude [nth count]]))

(deftype TestSeq [-list]
  ICounted
  (-count [this]
    (count -list))

  ISeqable
  (-seq [this] this)

  ISeq
  (-first [this]
    (nth -list 0))

  (-next [this]
    (if (= '() (rest -list))
      nil
      (TestSeq. (rest -list))))

  (-more [this]
    (loop [s (next (seq -list))
           r []]
      (if s
        (recur (next s) (conj r (first s)))
        r))))

(deftype TestSeqable [-list]
  ISeqable
  (-seq [this]
    (if (zero? (count -list))
      nil
      (TestSeq. -list))))

(defn test-seqable [-list]
  (TestSeqable. -list))

(defn test-seq [-list]
  (TestSeq. -list))
