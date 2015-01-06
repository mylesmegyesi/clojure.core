(ns clojure.next-test
  (:refer-clojure :only [apply deftype let map nil? true?])
  (:require [clojure.test           :refer :all]
            [clojure.next           :refer :all]
            [clojure.lang.protocols :refer [IPersistentVector]]))

(deftest constantly-test
  (testing "returns the return value"
    (is (= :val ((constantly :val)))))

  (testing "takes variable args and still returns the return value"
    (let [constant-fn (constantly :val)]
      (map #(is (= :val (apply constant-fn %)))
        [[] [1] [1 2] [1 2 3]]))))

(deftype TestVector []
  IPersistentVector)

(deftest vector?-test
  (testing "returns true if it is a vector"
    (is (vector? (vector))))

  (testing "returns true if the object implements IPersistentVector"
    (is (vector? (TestVector.))))

  (testing "returns false otherwise"
    (is (not (vector? #{})))
    (is (not (vector? '())))
    (is (not (vector? nil)))))

(deftest when-not-test
  (testing "returns nil if the test returns true"
    (is (nil? (when-not (= 1 1) :bar))))

  (testing "return the value if the test returns not true"
    (is (= :bar (when-not (= 1 2) :bar)))))
