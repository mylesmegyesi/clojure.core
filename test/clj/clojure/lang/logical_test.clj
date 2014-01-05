(ns clojure.lang.logical-test
  (:refer-clojure :only [])
  (:require [clojure.test            :refer :all]
            [clojure.lang.comparison :refer [=]]
            [clojure.lang.logical    :refer :all]))

(deftest not-test
  (testing "returns true if falsy"
    (is (= true (not false)))
    (is (= true (not nil))))

  (testing "returns false if truthy"
    (is (= false (not true)))
    (is (= false (not :something)))))
