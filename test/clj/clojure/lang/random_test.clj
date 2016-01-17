(ns clojure.lang.random-test
  (:refer-clojure :only [let some])
  (:require [clojure.test                 :refer :all]
            [clojure.next                 :refer :all]
            [clojure.lang.persistent-list :refer [list]]))

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
    (let [rands (clojure.core/repeatedly 20 #(rand 42))]
      (is (some #(>= % 1) rands)))))

(deftest rand-int-test
  (testing "returns an integer"
    (is (integer? (rand-int 1))))

  (testing "accepting an upper bound"
    (let [rands (clojure.core/repeatedly 20 #(rand-int 42))]
      (is (some #(>= % 1) rands)))))

(deftest rand-nth-test
  (testing "rand-nth returns the only entry for one element collections"
    (is (= :first (rand-nth (list :first)))))

  (testing "rand-nth returns one of a collections entries"
    (let [ret (rand-nth (list :first :second :third))]
      (is (or (= :first ret)
              (= :second ret)
              (= :third ret))))))

