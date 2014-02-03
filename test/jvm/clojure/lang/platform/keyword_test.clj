(ns clojure.lang.platform.keyword-test
  (:refer-clojure :only [let])
  (:require [clojure.test            :refer :all]
            [clojure.lang.hash       :refer [hash]]
            [clojure.lang.keyword    :refer [keyword]]
            [clojure.next            :refer :all]))

(deftest platform-equals-test
  (testing ".equals if ns and name are equal"
    (let [lhs (keyword "the-ns" "kwd")
          rhs (keyword "the-ns" "kwd")]
      (is (.equals lhs rhs))))

  (testing "not .equals if the names are not equal"
    (is (not (.equals (keyword "kwd1")
                      (keyword "kwd2"))))))

(deftest platform-compare-test
  (testing "returns 0 if the keywords are equal"
    (let [lhs (keyword "kwd")
          rhs (keyword "kwd")]
      (= 0 (.compareTo lhs rhs))))

  (testing "less than if lhs ns is nil and rhs ns is not nil"
    (let [lhs (keyword "kwd")
          rhs (keyword "the-ns" "kwd")]
      (= -1 (.compareTo lhs rhs))))

  (testing "greater than if lhs ns is not nil and rhs ns is nil"
    (let [lhs (keyword "the-ns1" "kwd")
          rhs (keyword "kwd")]
      (= 1 (.compareTo lhs rhs)))))

(deftest platform-hash-test
  (testing "returns the hashCode"
    (let [kwd (keyword "the-ns" "kwd")]
    (is (= (hash kwd)
           (.hashCode kwd))))))

(deftest platform-show-test
  (testing "returns the toString"
    (let [kwd (keyword "the-ns" "kwd")]
    (is (= ":the-ns/kwd"
           (.toString kwd))))))
