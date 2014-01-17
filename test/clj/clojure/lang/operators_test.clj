(ns clojure.lang.operators-test
  (:refer-clojure :only [reify let nil?])
  (:require [clojure.test             :refer :all]
            [clojure.lang.operators   :refer [and or = == not not= not==]]
            [clojure.lang.test-helper :refer [equal-type equivalent-type]]))

(deftest and-test
  (testing "returns true with zero arguments"
    (is (and)))

  (testing "true if the expression is true"
    (is (and true)))

  (testing "returns false if the expressions is false"
    (is (not (and false))))

  (testing "does not evaulate unecessary expressions"
    (is (not (and false (is false))))))

(deftest not-test
  (testing "returns true if falsy"
    (is (= true (not false)))
    (is (= true (not nil))))

  (testing "returns false if truthy"
    (is (= false (not true)))
    (is (= false (not :something)))))

(deftest or-test
  (testing "returns nil with zero arguments"
    (is (nil? (or))))

  (testing "true if the expression is true"
    (is (or true)))

  (testing "false if the expression is false"
    (is (not (or false))))

  (testing "does not evaluate unecessary expressions"
    (is (or true (is false)))))

(deftest =-test
  (testing "calls the -equal? method on the lhs"
    (is (= (equal-type true)
           :anything))
    (is (not (= (equal-type false)
                :anything))))

  (testing "not equal if either is nil"
    (is (not= nil :something))
    (is (not= :something nil)))

  (testing "equal if both are nil"
    (is (= nil nil)))

  (testing "true if only one item is given"
    (is (= :one-item)))

  (testing "more than two items -  true if every item is equal to each other"
    (is (= 1 1 1))
    (is (not (= 1 1 2)))
    (is (= 1 1 1 1))
    (is (not (= 1 1 1 2))))

  )

(deftest ^:new ==-test
  (testing "calls the -equivalent? method on the lhs"
    (is (== (equivalent-type true)
            :anything))
    (is (not (== (equivalent-type false)
                 :anything))))

  (testing "not equal if either is nil"
    (is (not== nil :something))
    (is (not== :something nil)))

  (testing "equal if both are nil"
    (is (== nil nil)))

  (testing "true if only one item is given"
    (is (== :something)))

  (testing "more than two items -  true if every item is equivalent to each other"
    (let [item1 (equivalent-type true)
          item2 (equivalent-type true)
          item3 (equivalent-type true)
          item4 (equivalent-type false)
          item5 (equivalent-type false)]
      (is (== item1 item2 item3))
      (is (== item1 item2 item4))
      (is (not== item1 item4 item2))
      (is (not== item1 item4 item5))))

  )
