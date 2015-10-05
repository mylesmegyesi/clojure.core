(ns clojure.lang.persistent-list-test
  (:refer-clojure :only [])
  (:require [clojure.test                 :refer :all]
            [clojure.lang.persistent-list :refer [list]]
            [clojure.support.test-seq     :refer [test-seq]]
            [clojure.next                 :refer :all]))

(deftest empty-list-equals-test
  (testing "empty list is equal to another empty list"
    (is (true? (.equals (list) (list)))))

  (testing "empty list is equal to another empty sequential"
    (is (true? (.equals (list) (test-seq '())))))

  (testing "an empty list is not equal to a non-empty list"
    (is (false? (.equals (list) (list :foo))))))

(deftest list-equals-test
  (testing "lists with items are equal to each other"
    (is (true? (.equals (list 1 2 3) (list 1 2 3)))))

  (testing "lists without equal items are not equal"
    (is (false? (.equals (list 1 2 3) (list 1))))))
