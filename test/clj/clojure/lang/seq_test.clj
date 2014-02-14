(ns clojure.lang.seq-test
  (:refer-clojure :only [let >])
  (:require [clojure.test             :refer :all]
            [clojure.next             :refer :all]
            [clojure.support.test-seq :refer [test-seq test-seqable]]))

(deftest every?-test
  (testing "returns true if the seq is nil"
    (is (every? #() nil)))

  (testing "returns true if every element passes the predicate test"
    (let [pred #(> % 0)
          s (test-seq '(1 2 3))]
      (is (every? pred s))))

  (testing "returns false if any element fails the predicate test"
    (let [pred #(> % 0)
          s (test-seq '(1 -1 2))]
      (is (not (every? pred s))))))

(deftest empty?-test
  (testing "returns true if the seq of the seqable is nil"
    (let [seqable (test-seqable '())]
      (is (empty? seqable))))

  (testing "returns false if the seq of the seqable has an item"
    (let [seqable (test-seqable '(1))]
      (is (not (empty? seqable))))))
