(ns clojure.lang.comparable-test
  (:refer-clojure :only [deftype])
  (:require [clojure.test             :refer :all]
            [clojure.lang.comparable  :refer [compare]]
            [clojure.lang.equivalence :refer [=]]
            [clojure.lang.icomparable :refer [IComparable]]))

(deftype Apple []
  IComparable
  (-compare-to [this other]
    10))

(deftest compare-test
  (testing "returns -1 if lhs is nil"
    (is (= -1 (compare nil :something))))

  (testing "returns 1 if rhs is nil"
    (is (= 1 (compare :something nil))))

  (testing "calls -compare-to on the lhs"
    (is (= 10 (compare (Apple.) :something))))

  )
