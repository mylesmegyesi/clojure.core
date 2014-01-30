(ns clojure.lang.ratio-test
  (:refer-clojure :only [let])
  (:require [clojure.test           :refer :all]
            [clojure.lang.operators :refer [not =]]
            [clojure.lang.ratio     :refer :all]))

(deftest ratio-test
  (testing "ratio? identifies a ratio"
    (let [ratio (make-ratio 1 2)]
      (is (ratio? ratio))
      (is (not (ratio? nil)))))

  (testing "numerator of a ratio"
    (let [ratio (make-ratio 1 2)]
      (is (= 1 (numerator ratio)))))

  (testing "denominator of a ratio"
    (let [ratio (make-ratio 1 2)]
      (is (= 2 (denominator ratio))))))
