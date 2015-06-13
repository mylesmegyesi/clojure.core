(ns clojure.lang.nil-test
  (:refer-clojure :only [true? false?])
  (:require [clojure.test                 :refer :all]
            [clojure.next                 :refer :all]
            [clojure.lang.persistent-list :refer [EMPTY-LIST]]))

(deftest nil?-test
  (testing "returns true if nil"
    (is (true? (nil? nil))))

  (testing "returns false otherwise"
    (is (false? (nil? :not-nil)))))

(deftest nil-test
  (testing "count of nil is zero"
    (is (zero? (count nil))))

  (testing "seq of nil is nil"
    (is (nil? (seq nil))))

  (testing "first of nil is nil"
    (is (nil? (first nil))))

  (testing "next of nil is nil"
    (is (nil? (next nil))))

  (testing "rest of nil is an empty list"
    (is (= EMPTY-LIST (rest nil))))

  (testing "nth of nil without a default is nil"
    (is (nil? (nth nil 0)))
    (is (nil? (nth nil 42))))

  (testing "nth of nil with a default is the default"
    (is (= "foo" (nth nil 0 "foo")))
    (is (= "foo" (nth nil 42 "foo"))))

  (testing "count of nil is 0"
    (is (zero? (count nil))))

  (testing "the type of nil is nil"
    (is (nil? (type nil)))))
