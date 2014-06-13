(ns clojure.lang.satisfies-test
  (:refer-clojure :only [bigint biginteger deftype double float])
  (:require [clojure.test           :refer :all]
            [clojure.next           :refer :all]
            [clojure.lang.protocols :refer :all]))

(deftype TestRatio []
  IRatio)

(deftest ratio?-test
  (testing "returns true if the object is a Ratio"
    (is (ratio? (/ 3 2))))

  (testing "returns true if satisfies IRatio"
    (is (ratio? (TestRatio.))))

  (testing "returns false otherwise"
    (is (not (ratio? (/ 1 1))))))

(deftype TestInteger []
  IInteger)

(deftest integer?-test
  (testing "returns true if the object is a Byte"
    (is (integer? (byte 0))))

  (testing "returns true if the object is a Short"
    (is (integer? (short 0))))

  (testing "returns true if the object is an Integer"
    (is (integer? (int 0))))

  (testing "returns true if the object is a long"
    (is (integer? (long 0))))

  (testing "returns true if the object is a BigInteger"
    (is (integer? (biginteger 0))))

  (testing "returns true if the object is a BigInt"
    (is (integer? (bigint 0))))

  (testing "returns true if satisfies IInteger"
    (is (integer? (TestInteger.))))

  (testing "returns false otherwise"
    (is (not (integer? 1.1)))))

(deftype TestDecimal []
  IDecimal)

(deftest decimal?-test
  (testing "returns true if the object is BigDecimal"
    (is (decimal? 0.0M)))

  (testing "returns true if satisfies IDecimal"
    (is (decimal? (TestDecimal.))))

  (testing "returns false otherwise"
    (is (not (decimal? 0)))))

(deftype TestFloat []
  IFloat)

(deftest float?-test
  (testing "returns true if the object is a Float"
    (is (float? (float 1.1))))

  (testing "returns true if the object is a double"
    (is (float? (double 1.1))))

  (testing "returns true if satisfies IFloat"
    (is (float? (TestFloat.))))

  (testing "returns false otherwise"
    (is (not (float? 0)))))

(deftest rational?-test
  (testing "returns true if the object satisfies IInteger"
    (is (rational? (TestInteger.))))

  (testing "returns true if the object satisfies IRatio"
    (is (rational? (TestRatio.))))

  (testing "returns true if the object satisfies IDecimal"
    (is (rational? (TestDecimal.))))

  (testing "returns false otherwise"
    (is (not (rational? (TestFloat.))))))
