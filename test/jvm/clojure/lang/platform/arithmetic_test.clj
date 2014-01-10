(ns clojure.lang.platform.arithmetic-test
  (:refer-clojure :only [defmacro let])
  (:require [clojure.test                 :refer :all]
            [clojure.lang.arithmetic      :refer [+ - * / mod exp]]
            [clojure.lang.operators       :refer [=]]
            [clojure.lang.platform.object :refer [instance?]])
  (:import [java.lang Byte Short Integer Float Double]
           [java.math BigInteger BigDecimal]
           [java.util.concurrent.atomic AtomicInteger AtomicLong]
           [clojure.lang.platform NumberOps]))

(defmacro plus-test [one neg-one]
  `(let [one# ~one
         neg-one# ~neg-one]
    (testing "with a Byte"
      (is (= 2 (+ one# (Byte. "1"))))
      (is (= 128 (+ one# Byte/MAX_VALUE)))
      (is (= -129 (+ neg-one# Byte/MIN_VALUE))))

    (testing "with a Short"
      (is (= 2 (+ one# (Short. "1"))))
      (is (= 32768 (+ one# Short/MAX_VALUE)))
      (is (= -32769 (+ neg-one# Short/MIN_VALUE))))

    (testing "with an Integer"
      (is (= 2 (+ one# (Integer. "1"))))
      (is (= 2147483648 (+ one# Integer/MAX_VALUE)))
      (is (= -2147483649 (+ neg-one# Integer/MIN_VALUE))))

    (testing "with a Long"
      (is (= 2 (+ one# (Long. "1"))))
      (is (= (BigInteger. "9223372036854775808") (+ one# Long/MAX_VALUE)))
      (is (= (BigInteger. "-9223372036854775809") (+ neg-one# Long/MIN_VALUE))))

    (testing "with a BigInteger"
      (is (= 2 (+ one# (BigInteger. "1"))))
      (is (= (BigInteger. "9223372036854775809") (+ one# (BigInteger. "9223372036854775808"))))
      (is (= (BigInteger. "-9223372036854775810") (+ neg-one# (BigInteger. "-9223372036854775809")))))

    ))

(deftest byte-plus-test
  (plus-test (Byte. "1") (Byte. "-1")))

(deftest short-plus-test
  (plus-test (Short. "1") (Short. "-1")))

(deftest int-plus-test
  (plus-test (Integer. "1") (Integer. "-1")))

(deftest long-plus-test
  (plus-test (Long. "1") (Long. "-1")))

(deftest bigint-plus-test
  (plus-test (BigInteger. "1") (BigInteger. "-1")))
