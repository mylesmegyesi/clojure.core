(ns clojure.lang.seq-test
  (:refer-clojure :only [defmacro fn let list list* nil? >])
  (:require [clojure.test             :refer :all]
            [clojure.next             :refer :all]
            [clojure.lang.exceptions  :refer [out-of-bounds-exception]]
            [clojure.support.test-seq :refer [test-seq test-seqable]]))

(defmacro out-of-bounds-exception-thrown? [& body]
  (list 'is (list* 'thrown? out-of-bounds-exception body)))

(deftest second-test
  (testing "second of nil is nil"
    (is (nil? (second nil))))

  (testing "second of a seq with one element is nil"
    (is (nil? (second (test-seqable '(1))))))

  (testing "second of a two element seq is the last element"
    (is (= 2 (second (test-seqable '(1 2))))))

  (testing "second of a many element seq is the second element"
    (is (= 2 (second (test-seqable '(1 2 3 4)))))))

(deftest last-test
  (testing "last of nil is nil"
    (is (nil? (last nil))))

  (testing "last of an empty seq is nil"
    (is (nil? (last (test-seqable (list))))))

  (testing "last of a one element seq is the element"
    (is (= 1 (last (test-seqable (list 1))))))

  (testing "last of a many element seq is the last element"
    (is (= 3 (last (test-seqable (list 1 2 3)))))))

(deftest ffirst-test
  (testing "ffirst of nil is nil"
    (is (nil? (ffirst nil))))

  (testing "ffirst of an empty list or list with an empty list is nil"
    (is (nil? (ffirst (test-seqable (list)))))
    (is (nil? (ffirst (test-seqable (list (list)))))))

  (testing "ffirst return the first of the first"
    (is (= :first (ffirst (test-seqable (list (list :first))))))))

(deftest nfirst-test
  (testing "nfirst of nil is nil"
    (is (nil? (nfirst nil))))

  (testing "next first of a list in a list"
    (is (= 2 (first (nfirst (test-seqable (list (list 1 2)))))))))

(deftest nnext-test
  (testing "nnext of nil is nil"
    (is (nil? (nnext nil))))

  (testing "nnext of a seq with zero, one or two elements is nil"
    (is (nil? (nnext (test-seqable '()))))
    (is (nil? (nnext (test-seqable '(1)))))
    (is (nil? (nnext (test-seqable '(1 2))))))

  (testing "nnext of a seq with three or more elements is a seq remainder"
    (is (= 3 (first (nnext (test-seqable '(1 2 3))))))
    (is (= 4 (second (nnext (test-seqable '(1 2 3 4))))))))

(deftest fnext-test
  (testing "fnext of nil is nil"
    (is (nil? (fnext nil))))

  (testing "first of the next"
    (is (= 2 (fnext (test-seqable '(1 2)))))))

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

(deftest nth-test
  (testing "find the nth of a seq"
    (let [s (test-seqable '(1 2 3))]
      (is (= 2 (nth s 1)))))

  (testing "raise an out of bounds exception when finding the nth larger than seq size"
    (let [s (test-seqable '(1 2 3))]
      (out-of-bounds-exception-thrown? (nth s 42))))

  (testing "find the nth of a seq with a default"
    (let [s (test-seqable '(1 2 3))]
      (is (= 2 (nth s 1 "not found")))))

  (testing "return default when finding the nth larger than seq size"
    (let [s (test-seqable '(1 2 3))]
      (is (= "not found" (nth s 42 "not found"))))))

