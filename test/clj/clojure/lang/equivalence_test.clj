(ns clojure.lang.equivalence-test
  (:refer-clojure :only [constantly let])
  (:require [clojure.test                    :refer :all]
            [clojure.lang.equivalence-helper :refer :all]
            [clojure.lang.equivalence        :refer :all]
            [clojure.lang.platform.object    :refer [identical?]]))

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
    (is (= false (not (new-apple nil nil))))))
