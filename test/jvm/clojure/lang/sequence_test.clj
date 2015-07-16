(ns clojure.lang.sequence-test
  (:refer-clojure :only [let])
  (:require [clojure.test                         :refer :all]
            [clojure.next                         :refer :all]
            [clojure.lang.persistent-list         :refer [EMPTY-LIST]]
            [clojure.lang.protocols               :refer [-index]]
            [clojure.support.exception-assertions :refer [argument-error-is-thrown?]]))

(deftest platform-seq-test
  (testing "an argument error is thrown without a seqable type"
    (argument-error-is-thrown? #"Don't know how to create ISeq from: java.lang.Long" (seq 1))))

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

