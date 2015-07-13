(ns clojure.lang.random-test
  (:refer-clojure :only [let distinct repeatedly some])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all :exclude [repeatedly]]))

(deftest rand-test
  (testing "returns a floating point number"
    (is (float? (rand))))

  (testing "is between 0 and 1"
    (let [rands (repeatedly 20 #(rand))]
      (is (every? #(and (>= % 0) (<= % 1)) rands))))

  (testing "is pseudo-random"
    (let [rands (repeatedly 20 #(rand))]
      (is (= rands (distinct rands)))))

  (testing "accepting an upper bound"
    (let [rands (repeatedly 20 #(rand 42))]
      (is (some #(>= % 1) rands)))))

(deftest rand-int-test
  (testing "returns an integer"
    (is (integer? (rand-int 1))))

  (testing "accepting an upper bound"
    (let [rands (repeatedly 20 #(rand-int 42))]
      (is (some #(>= % 1) rands)))))
