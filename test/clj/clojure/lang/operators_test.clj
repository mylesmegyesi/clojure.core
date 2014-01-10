(ns clojure.lang.operators-test
  (:refer-clojure :only [deftype constantly let])
  (:require [clojure.test                  :refer :all]
            [clojure.lang.operators        :refer :all]
            [clojure.lang.operators-helper :refer :all]
            [clojure.lang.icomparable      :refer [IComparable]]
            [clojure.lang.platform.object  :refer [identical?]]))

(deftype CApple []
  IComparable
  (-compare-to [this other]
    10))

(deftest compare-test
  (testing "returns -1 if lhs is nil"
    (is (= -1 (compare nil :something))))

  (testing "returns 1 if rhs is nil"
    (is (= 1 (compare :something nil))))

  (testing "calls -compare-to on the lhs"
    (is (= 10 (compare (CApple.) :something))))

  )

(def always-equal (constantly true))
(def always-inequal (constantly false))

(deftest =-test
  (testing "lhs and rhs are equal if the lhs says so"
    (is (= (new-apple always-equal nil)
           (new-orange nil nil)))
    (is (not= (new-apple always-inequal nil)
           (new-orange nil nil))))

  (testing "not equal if either is nil"
    (is (not= nil (new-orange nil nil)))
    (is (not= (new-orange nil nil) nil)))

  (testing "equal if both are nil"
    (is (= nil nil)))

  (testing "true if only one item is given"
    (is (= (new-apple nil nil))))

  (testing "more than two items -  true if every item is equal to the first"
    (let [item3 (new-apple always-equal nil)
          if-not-item3 #(not (identical? %2 item3))
          item1 (new-apple if-not-item3 nil)
          item2 (new-apple always-equal nil)
          item4 (new-apple nil nil)
          item5 (new-apple nil nil)]
      (is (not= item1 item2 item3))
      (is (= item1 item2 item4))
      (is (= item1 item2 item4 item5))
      (is (= item2 item1 item3 item4 item5))))

  )

(deftest ==-test
  (testing "lhs and rhs are equal if the lhs says so and types are equal"
    (is (not== (new-apple always-equal nil)
                (new-orange nil nil)))
    (is (== (new-apple always-equal nil)
             (new-apple nil nil)))
    (is (not== (new-apple always-inequal nil)
                (new-apple nil nil))))

  (testing "not equal if either is nil"
    (is (not== nil (new-orange nil nil)))
    (is (not== (new-orange nil nil) nil)))

  (testing "equal if both are nil"
    (is (== nil nil)))

  (testing "true if only one item is given"
    (is (== (new-apple nil nil))))

  (testing "more than two items -  true if every item is strictly equal to the first"
    (let [item1 (new-apple always-equal nil)
          item2 (new-apple always-equal nil)
          item3 (new-orange nil nil)
          item4 (new-apple nil nil)
          item5 (new-apple nil nil)]
      (is (not== item1 item2 item3))
      (is (== item1 item2 item4))
      (is (== item1 item2 item4 item5))
      (is (== item2 item1 item4 item5))))

  )

(deftest not-test
  (testing "returns true if falsy"
    (is (= true (not false)))
    (is (= true (not nil))))

  (testing "returns false if truthy"
    (is (= false (not true)))
    (is (= false (not :something)))))

(deftest and-test
  (testing "returns true with zero arguments"
    (is (and)))

  (testing "true if the expression is true"
    (is (and true)))

  (testing "returns false if the expressions is false"
    (is (not (and false))))

  (testing "does not evaulate unecessary expressions"
    (is (not (and false (is false))))))
