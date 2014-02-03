(ns clojure.lang.platform.symbol-test
  (:refer-clojure :only [let])
  (:require [clojure.test           :refer :all]
            [clojure.lang.hash      :refer [hash]]
            [clojure.lang.symbol    :refer [symbol]]
            [clojure.next           :refer :all]))

(deftest platform-equals-test
  (testing ".equals if ns and name are equal"
    (let [lhs (symbol "the-ns" "sym")
          rhs (symbol "the-ns" "sym")]
      (is (.equals lhs rhs))))

  (testing "not .equals if the names are not equal"
    (is (not (.equals (symbol "sym1")
                      (symbol "sym2"))))))

(deftest platform-compare-test
  (testing "returns 0 if the symbols are equal"
    (let [lhs (symbol "sym")
          rhs (symbol "sym")]
      (= 0 (.compareTo lhs rhs))))

  (testing "less than if lhs ns is nil and rhs ns is not nil"
    (let [lhs (symbol "sym")
          rhs (symbol "the-ns" "sym")]
      (= -1 (.compareTo lhs rhs))))

  (testing "greater than if lhs ns is not nil and rhs ns is nil"
    (let [lhs (symbol "the-ns1" "sym")
          rhs (symbol "sym")]
      (= 1 (.compareTo lhs rhs)))))

(deftest platform-hash-test
  (testing "returns the hashCode"
    (let [sym (symbol "the-ns" "sym")]
    (is (= (hash sym)
           (.hashCode sym))))))

(deftest platform-show-test
  (testing "returns the toString"
    (let [sym (symbol "the-ns" "sym")]
    (is (= "the-ns/sym"
           (.toString sym))))))
