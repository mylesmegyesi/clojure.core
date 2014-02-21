(ns clojure.lang.seq-test
  (:refer-clojure :only [fn let nil? > +])
  (:require [clojure.test             :refer :all]
            [clojure.next             :refer :all :exclude [+]]
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

(deftest reduce-test
  (testing "returns the result of invoking the function with zero arguments when the collection is nil"
    (is (= :foo (reduce (fn [] :foo) nil))))

  (testing "returns the start value if the collection is nil when supplied a starting value"
    (is (= :bar (reduce (fn [] :foo) :bar nil))))

  (testing "reduces the collection without a supplied start value"
    (is (= 6 (reduce + (test-seqable '(1 2 3))))))

  (testing "reduces the collection with a supplied start value"
    (is (= 10 (reduce + 1 (test-seqable '(2 3 4)))))))
