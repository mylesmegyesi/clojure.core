(ns clojure.lang.out-test
  (:refer-clojure :only [let nil? reify])
  (:require [clojure.test           :refer :all]
            [clojure.next           :refer :all]
            [clojure.lang.protocols :refer [IMeta]]))

(deftest print-simple-test
  (testing "writes the to string version of an obj"
    (let [o (reify
              IMeta
              (-meta [_] nil))]
      (is (=
            (with-out-str (print-simple o *out*))
            (str o))))))

(deftest pr-test
  (testing "print for a :default meta type that is not an IObj"
    (let [obj (reify
                IMeta
                (-meta [_] (array-map :type :default)))]
      (is (=
            (with-out-str (pr obj))
            (str obj))))))

(deftest newline-test
  (testing "newline returns nil"
    (with-out-str
      (is (nil? (newline))))))

(deftest flush-test
  (testing "flush returns nil"
    (with-out-str
      (is (nil? (flush))))))

