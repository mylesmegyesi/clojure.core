(ns clojure.lang.comparable-test
  (:refer-clojure :only [])
  (:require [clojure.test             :refer :all]
            [clojure.lang.comparable  :refer [compare]]
            [clojure.lang.equivalence :refer [=]]))

(deftest compare-test
  (testing "returns -1 if lhs is nil"
    (is (= -1 (compare nil :something))))

  (testing "returns 1 if rhs is nil"
    (is (= 1 (compare :something nil))))

  )
