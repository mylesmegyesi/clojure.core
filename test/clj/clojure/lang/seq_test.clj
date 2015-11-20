(ns clojure.lang.seq-test
  (:refer-clojure :only [defmacro fn let list* reify])
  (:require [clojure.test                         :refer :all]
            [clojure.next                         :refer :all]
            [clojure.lang.protocols               :refer [ISeq]]
            [clojure.lang.persistent-list         :refer [list]]
            [clojure.support.exception-assertions :refer [class-cast-exception-is-thrown?
                                                          out-of-bounds-exception-is-thrown?]]))

(deftest seq?-test
  (testing "returns true for a seq"
    (is (seq? (reify ISeq))))

  (testing "returns false otherwise"
    (is (not (seq? :foo)))))

(deftest second-test
  (testing "second of nil is nil"
    (is (nil? (second nil))))

  (testing "second of a seq with one element is nil"
    (is (nil? (second (list 1)))))

  (testing "second of a two element seq is the last element"
    (is (= 2 (second (list 1 2)))))

  (testing "second of a many element seq is the second element"
    (is (= 2 (second (list 1 2 3 4))))))

(deftest last-test
  (testing "last of nil is nil"
    (is (nil? (last nil))))

  (testing "last of an empty seq is nil"
    (is (nil? (last (list)))))

  (testing "last of a one element seq is the element"
    (is (= 1 (last (list 1)))))

  (testing "last of a many element seq is the last element"
    (is (= 3 (last (list 1 2 3))))))

(deftest ffirst-test
  (testing "ffirst of nil is nil"
    (is (nil? (ffirst nil))))

  (testing "ffirst of an empty list or list with an empty list is nil"
    (is (nil? (ffirst (list))))
    (is (nil? (ffirst (list (list))))))

  (testing "ffirst return the first of the first"
    (is (= :first (ffirst (list (list :first)))))))

(deftest nfirst-test
  (testing "nfirst of nil is nil"
    (is (nil? (nfirst nil))))

  (testing "next first of a list in a list"
    (is (= 2 (first (nfirst (list (list 1 2))))))))

(deftest nnext-test
  (testing "nnext of nil is nil"
    (is (nil? (nnext nil))))

  (testing "nnext of a seq with zero, one or two elements is nil"
    (is (nil? (nnext (list))))
    (is (nil? (nnext (list 1))))
    (is (nil? (nnext (list 1 2)))))

  (testing "nnext of a seq with three or more elements is a seq remainder"
    (is (= 3 (first (nnext (list 1 2 3)))))
    (is (= 4 (second (nnext (list 1 2 3 4)))))))

(deftest fnext-test
  (testing "fnext of nil is nil"
    (is (nil? (fnext nil))))

  (testing "first of the next"
    (is (= 2 (fnext (list 1 2))))))

(deftest every?-test
  (testing "returns true if the seq is nil"
    (is (every? #() nil)))

  (testing "returns true if every element passes the predicate test"
    (let [pred #(> % 0)
          s (list 1 2 3)]
      (is (every? pred s))))

  (testing "returns false if any element fails the predicate test"
    (let [pred #(> % 0)
          s (list 1 -1 2)]
      (is (not (every? pred s))))))

(deftest empty?-test
  (testing "returns true if the seq of the seqable is nil"
    (let [seqable (list)]
      (is (empty? seqable))))

  (testing "returns false if the seq of the seqable has an item"
    (let [seqable (list 1)]
      (is (not (empty? seqable))))))

(deftest reduce-test
  (testing "returns the result of invoking the function with zero arguments when the collection is nil"
    (is (= :foo (reduce (fn [] :foo) nil))))

  (testing "returns the start value if the collection is nil when supplied a starting value"
    (is (= :bar (reduce (fn [] :foo) :bar nil))))

  (testing "reduces the collection without a supplied start value"
    (is (= 6 (reduce + (list 1 2 3)))))

  (testing "reduces the collection with a supplied start value"
    (is (= 10 (reduce + 1 (list 2 3 4))))))

(deftest map-test
  (testing "map over nil returns an empty result"
    (is (empty? (map identity nil))))

  (testing "map over a seqable collection returns a lazy seq"
    (let [res (map identity (vector 1 2 3))]
      (is (not (realized? res)))
      (is (= 3 (count res)))
      (is (realized? res)))))

(deftest nth-test
  (testing "find the nth of a seq"
    (let [s (list 1 2 3)]
      (is (= 2 (nth s 1)))))

  (testing "raise an out of bounds exception when finding the nth larger than seq size"
    (let [s (list 1 2 3)]
      (out-of-bounds-exception-is-thrown? #".*" (nth s 42))))

  (testing "find the nth of a seq with a default"
    (let [s (list 1 2 3)]
      (is (= 2 (nth s 1 "not found")))))

  (testing "return default when finding the nth larger than seq size"
    (let [s (list 1 2 3)]
      (is (= "not found" (nth s 42 "not found"))))))

(deftest iterator-seq-test
  (testing "throws an exception when not given an iterator"
    (class-cast-exception-is-thrown? #".*" (iterator-seq "foo"))))

(deftest line-seq-test
  (testing "throws an exception when not given a reader"
    (class-cast-exception-is-thrown? #".*" (line-seq "foo"))))

(deftest sort-test
  (testing "returns empty seq if argument is nil"
    (is (= (sort nil) (list))))

  (testing "sorting an empty seq returns an empty seq"
    (is (= (sort (list)) (list))))

  (testing "sorting a one element seq is equal to itself"
    (is (= (sort (list 1)) (list 1))))

  (testing "sorting an unsorted two element seq with default comparator"
    (is (= (sort (list 2 1)) (list 1 2))))

  (testing "sorting an unsorted two element seq with custom comparator"
    (is (= (sort #(> %1 %2) (list 1 2)) (list 2 1)))))

