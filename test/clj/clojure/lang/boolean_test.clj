(ns clojure.lang.boolean-test
  (:refer-clojure :only [])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all]))

(deftest true?-test
  (testing "returns true if true"
    (is (= true (true? true))))

  (testing "returns false otherwise"
    (is (= false (true? false)))))

(deftest false?-test
  (testing "returns true if false"
    (is (= true (false? false))))

  (testing "returns false otherwise"
    (is (= false (false? true)))))

