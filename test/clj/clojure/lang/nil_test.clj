(ns clojure.lang.nil-test
  (:refer-clojure :only [nil?])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all]))

(deftest nil-test
  (testing "count of nil is zero"
    (is (zero? (count nil))))

  (testing "seq of nil is nil"
    (is (nil? (seq nil))))

  (testing "first of nil is nil"
    (is (nil? (first nil))))

  (testing "nth of nil without a default is nil"
    (is (nil? (nth nil 0)))
    (is (nil? (nth nil 42))))

  (testing "nth of nil with a default is the default"
    (is (= "foo" (nth nil 0 "foo")))
    (is (= "foo" (nth nil 42 "foo")))))
