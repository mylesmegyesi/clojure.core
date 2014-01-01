(ns clojure.lang.platform.equivalence-test
  (:refer-clojure :only [defmacro])
  (:require [clojure.test             :refer :all]
            [clojure.lang.equivalence :refer [= not=]])
  (:import [java.lang Byte Short Integer Float Double]
           [java.math BigInteger BigDecimal]
           [java.util.concurrent.atomic AtomicInteger AtomicLong]))

(defmacro number-equality-test [neg-one zero one]
  `(do
    (testing "equality with another Byte"
      (is (= ~neg-one (.byteValue (Byte. "-1"))))
      (is (= ~zero (Byte. "0")))
      (is (not= ~one :one))
      (is (not= ~one (Byte. "2"))))

    (testing "equality with a Short"
      (is (= ~neg-one (.shortValue (Short. "-1"))))
      (is (= ~zero (Short. "0")))
      (is (not= ~one :one))
      (is (not= ~one (Short. "2"))))

    (testing "equality with an Integer"
      (is (= ~neg-one (.intValue (Integer. "-1"))))
      (is (= ~zero (Integer. "0")))
      (is (not= ~one :one))
      (is (not= ~one (Integer. "2"))))

    (testing "equality with a Long"
      (is (= ~neg-one (.longValue (Long. "-1"))))
      (is (= ~zero (Long. "0")))
      (is (not= ~one :one))
      (is (not= ~one (Long. "2"))))

    (testing "equality with a Float"
      (is (= ~neg-one (.floatValue (Float. "-1.0"))))
      (is (= ~zero (Float. "0.0")))
      (is (not= ~one :one))
      (is (not= ~one (Float. "1.1"))))

    (testing "equality with a Double"
      (is (= ~neg-one (.doubleValue (Double. "-1.0"))))
      (is (= ~zero (Double. "0.0")))
      (is (not= ~one :one))
      (is (not= ~one (Double. "1.1"))))

    (testing "equality with a BigInteger"
      (is (= ~neg-one (BigInteger. "-1")))
      (is (= ~zero (BigInteger. "0")))
      (is (not= ~one :one))
      (is (not= ~one (BigInteger. "2"))))

    (testing "equality with a BigDecimal"
      (is (= ~neg-one (BigDecimal. "-1.0")))
      (is (= ~zero (BigDecimal. "0.0")))
      (is (not= ~one :one))
      (is (not= ~one (BigDecimal. "1.1"))))))

(deftest byte-equality-test
  (number-equality-test
    (Byte. "-1")
    (Byte. "0")
    (Byte. "1")))

(deftest short-equality-test
  (number-equality-test
    (Short. "-1")
    (Short. "0")
    (Short. "1")))

(deftest integer-equality-test
  (number-equality-test
    (Integer. "-1")
    (Integer. "0")
    (Integer. "1")))

(deftest atomic-integer-equality-test
  (number-equality-test
    (AtomicInteger. -1)
    (AtomicInteger. 0)
    (AtomicInteger. 1)))

(deftest long-equality-test
  (number-equality-test
    (Long. "-1")
    (Long. "0")
    (Long. "1")))

(deftest atomic-long-equality-test
  (number-equality-test
    (AtomicLong. -1)
    (AtomicLong. 0)
    (AtomicLong. 1)))

(deftest float-equality-test
  (number-equality-test
    (Float. "-1.0")
    (Float. "0.0")
    (Float. "1.0")))

(deftest double-equality-test
  (number-equality-test
    (Double. "-1.0")
    (Double. "0.0")
    (Double. "1.0")))

(deftest bigint-equality-test
  (number-equality-test
    (BigInteger. "-1")
    (BigInteger. "0")
    (BigInteger. "1")))

(deftest bigdec-equality-test
  (number-equality-test
    (BigDecimal. "-1")
    (BigDecimal. "0")
    (BigDecimal. "1")))
