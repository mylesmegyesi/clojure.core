(ns clojure.lang.seq-test
  (:refer-clojure :only [count deftype let nth rest zero? >])
  (:require [clojure.test           :refer :all]
            [clojure.lang.operators :refer [not =]]
            [clojure.lang.seq       :refer :all]
            [clojure.lang.iseq      :refer [ISeq]]
            [clojure.lang.iseqable  :refer [ISeqable]]))

(deftype TestSeq [-list]
  ISeqable
  (-seq [this] this)

  ISeq
  (-first [this]
    (nth -list 0))

  (-next [this]
    (if (= '() (rest -list))
      nil
      (TestSeq. (rest -list)))))

(deftype TestSeqable [-list]
  ISeqable
  (-seq [this]
    (if (zero? (count -list))
      nil
      (TestSeq. -list))))

(deftest every?-test
  (testing "returns true if the seq is nil"
    (is (every? #() nil)))

  (testing "returns true if every element passes the predicate test"
    (let [pred #(> % 0)
          s (TestSeq. '(1 2 3))]
      (is (every? pred s))))

  (testing "returns false if any element fails the predicate test"
    (let [pred #(> % 0)
          s (TestSeq. '(1 -1 2))]
      (is (not (every? pred s))))))

(deftest empty?-test
  (testing "returns true if the seq of the seqable is nil"
    (let [seqable (TestSeqable. '())]
      (is (empty? seqable))))

  (testing "returns false if the seq of the seqable has an item"
    (let [seqable (TestSeqable. '(1))]
      (is (not (empty? seqable))))))
