(ns clojure.lang.sequence-test
  (:refer-clojure :only [let])
  (:require [clojure.test                         :refer :all]
            [clojure.next                         :refer :all]
            [clojure.lang.persistent-list         :refer [EMPTY-LIST]]
            [clojure.lang.protocols               :refer [-index]]
            [clojure.support.exception-assertions :refer [argument-error-is-thrown?]]
            [clojure.support.test-seq             :refer [test-seq]])
  (:import [java.util ArrayList HashMap]))

(deftest platform-seq-test
  (testing "an argument error is thrown without a seqable type"
    (argument-error-is-thrown? #"Don't know how to create ISeq from: java.lang.Long" (seq 1))))

(def iter-seq (let [arr-list (ArrayList.)]
                (.add arr-list 1)
                (.add arr-list 2)
                (.add arr-list 3)
                (seq arr-list)))

(deftest iterator-seq-test
  (testing "count of an iterator seq"
    (let [s1 iter-seq
          s2 (next s1)
          s3 (next s2)]
      (is (= 3 (count s1)))
      (is (= 2 (count s2)))
      (is (= 1 (count s3)))))

  (testing "first of an iterator is the first element"
    (is (= 1 (first iter-seq))))

  (testing "next returns a seq with the rest of the iterator"
    (let [s (next iter-seq)]
      (is (= 2 (count s)))
      (is (= 2 (first s)))))

  (testing "next returns nil when there is no rest"
    (let [s (next (seq (ArrayList.)))]
      (is (nil? s))))

  (testing "rest returns a seq with the rest of the iterator"
    (let [s (rest iter-seq)]
      (is (= 2 (count s)))
      (is (= 2 (first s)))))

  (testing "rest returns the empty list when there is no rest"
    (let [s (rest (seq (ArrayList.)))]
      (is (= EMPTY-LIST s))))

  (testing "conj an element"
    (let [c (conj iter-seq 4)]
      (is (= 4 (first c)))
      (is (= 1 (second c)))))

  (testing "empty of an iterator seq is an empty list"
    (let [e (empty iter-seq)]
      (is (= EMPTY-LIST e))))

  (testing "iterator seq holds a meta value"
    (let [w-meta (with-meta iter-seq (array-map :so :meta))]
      (is (= (array-map :so :meta) (meta w-meta)))))

  (testing "iterator-seq can be invoked directly"
    (let [arr-list (ArrayList.)]
      (.add arr-list 1)
      (.add arr-list 2)
      (.add arr-list 3)
      (let [s (iterator-seq (.iterator ^Iterable arr-list))]
        (is (= 3 (count s)))))))

(deftest map-entry-set-iterator-seq-test
  (testing "a map's seq is an iterator seq"
    (let [hm (HashMap.)]
      (.put hm 1 1)
      (is (seq? (seq hm)))
      (is (= 1 (count hm))))))

(def arr-seq (seq (into-array (test-seq '(1 2 3)))))

(deftest array-seq-test
  (testing "count of an array seq"
    (let [s1 arr-seq
          s2 (next s1)
          s3 (next s2)]
      (is (= 3 (count s1)))
      (is (= 2 (count s2)))
      (is (= 1 (count s3)))))

  (testing "first of an array is the first element"
    (is (= 1 (first arr-seq))))

  (testing "next returns a seq with the rest of the array"
    (let [s (next arr-seq)]
      (is (= 2 (count s)))
      (is (= 2 (first s)))))

  (testing "next returns nil when there is no rest"
    (let [s (next (into-array (test-seq '())))]
      (is (nil? s))))

  (testing "rest returns a seq with the rest of the array"
    (let [s (rest arr-seq)]
      (is (= 2 (count s)))
      (is (= 2 (first s)))))

  (testing "rest returns the empty list when there is no rest"
    (let [s (rest (into-array (test-seq '(1))))]
      (is (= EMPTY-LIST s))))

  (testing "the current index"
    (let [s1 (seq arr-seq)
          s2 (next s1)
          s3 (next s2)]
      (is (= 0 (-index s1)))
      (is (= 1 (-index s2)))
      (is (= 2 (-index s3)))))

  (testing "conj an element"
    (let [c (conj arr-seq 4)]
      (is (= 4 (first c)))
      (is (= 1 (second c)))))

  (testing "empty of an array seq is an empty list"
    (let [e (empty arr-seq)]
      (is (= EMPTY-LIST e))))

  (testing "array seq holds a meta value"
    (let [w-meta (with-meta arr-seq (array-map :so :meta))]
      (is (= (array-map :so :meta) (meta w-meta))))))

(deftest string-seq-test
  (testing "count of a string seq"
    (let [s1 (seq "foo")
          s2 (next s1)
          s3 (next s2)]
      (is (= 3 (count s1)))
      (is (= 2 (count s2)))
      (is (= 1 (count s3)))))

  (testing "first of a string is the first character"
    (is (= \f (first (seq "first")))))

  (testing "next returns a seq with the rest of the string"
    (let [s (next "foo")]
      (is (= 2 (count s)))
      (is (= \o (first s)))))

  (testing "next returns nil when there is no rest"
    (let [s (next "f")]
      (is (nil? s))))

  (testing "rest returns a seq with the rest of the string"
    (let [s (rest "foo")]
      (is (= 2 (count s)))
      (is (= \o (first s)))))

  (testing "rest returns the empty list when there is no rest"
    (let [s (rest "f")]
      (is (= EMPTY-LIST s))))

  (testing "the current index"
    (let [s1 (seq "foo")
          s2 (next s1)
          s3 (next s2)]
      (is (= 0 (-index s1)))
      (is (= 1 (-index s2)))
      (is (= 2 (-index s3)))))

  (testing "conj an element"
    (let [c (conj (seq "ello") \h)]
      (is (= \h (first c)))
      (is (= \e (second c)))))

  (testing "empty of a string seq is an empty list"
    (let [e (empty (seq "foo"))]
      (is (= EMPTY-LIST e))))

  (testing "string seq holds a meta value"
    (let [w-meta (with-meta (seq "foo") (array-map :so :meta))]
      (is (= (array-map :so :meta) (meta w-meta))))))

