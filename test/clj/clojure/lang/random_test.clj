(ns clojure.lang.random-test
  (:refer-clojure :only [let distinct nil? repeatedly some take >= <=])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all :exclude [take]]))

(deftest rand-test
  (testing "returns a floating point number"
    (is (float? (rand))))

  (testing "is between 0 and 1"
    (let [rands (take 20 (repeatedly #(rand)))]
      (is (every? #(and (>= % 0) (<= % 1)) rands))))

  (testing "is pseudo-random"
    (let [rands (take 20 (repeatedly #(rand)))]
      (is (= rands (distinct rands)))))

  (testing "accepting an upper bound"
    (let [rands (take 20 (repeatedly #(rand 42)))]
      (is (some #(>= % 1) rands)))))

(deftest rand-int-test
  (testing "returns an integer"
    (is (integer? (rand-int 1))))

  (testing "accepting an upper bound"
    (let [rands (take 20 (repeatedly #(rand-int 42)))]
      (is (some #(>= % 1) rands)))))
