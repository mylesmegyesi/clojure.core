(ns clojure.next-test
  (:refer-clojure :only [*assert* apply binding defmacro deftype eval let list list* map nil? true? var-set])
  (:require [clojure.test            :refer :all]
            [clojure.next            :refer :all]
            [clojure.lang.exceptions :refer [assertion-error]]
            [clojure.lang.protocols  :refer [IPersistentVector]]))

(defmacro assertion-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? assertion-error msg body)))

(deftest clojure-version-test
  (testing "current clojure version as a string"
    (is (= "1.6.0" (clojure-version)))))

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

(deftest assert-test
  (testing "*assert* is true by default"
    (is (true? *assert*)))

  (testing "assert raises an assertion error if the test fails"
    (assertion-error-is-thrown? #"Assert failed" (assert (= 1 2))))

  (testing "assert does not raise an assertion error if the test fails"
    (is (nil? (assert (= 1 1)))))

  (testing "assert raises an assertion error with a message"
    (assertion-error-is-thrown? #"Assert failed: test" (assert (= 1 2) "test")))

  (testing "assert does not run if *assert* is bound to false"
    (is
      (nil? (binding [*assert* false] (eval '(assert false)))))))

