(ns clojure.lang.comparison-test
  (:refer-clojure :only [deftype let <])
  (:require [clojure.test            :refer :all]
            [clojure.lang.protocols  :refer [IComparable]]
            [clojure.next            :refer :all]))

(deftype TenComparator []
  IComparable
  (-compare-to [this other]
    10))

(deftest compare-test
  (testing "compare uses the internal compare method of the first argument"
    (is (= 10 (compare (TenComparator.) :foo))))

  (testing "returns 0 if the two instances are the same"
    (let [instance (TenComparator.)]
      (is (= 0 (compare instance instance)))
      (is (= 0 (compare nil nil)))))

  (testing "returns -1 if the first element is nil"
    (is (= -1 (compare nil (TenComparator.)))))

  (testing "returns 1 if the second element is nil"
    (is (= 1 (compare (TenComparator.) nil))))

  )

(deftest compare-with-numbers-test
  (testing "returns -1 if the first element is less than the second"
    (is (= -1 (compare 1 2)))
    (is (= -1 (compare 1 1.1)))
    (is (= -1 (compare 0.9 1.1))))

  (testing "returns 1 if the second element is less than the first"
    (is (= 1 (compare 2 1)))
    (is (= 1 (compare 1.1 1)))
    (is (= 1 (compare 1.1 0.9))))

  (testing "returns 0 if the two numbers are the same value"
    (is (= 0 (compare 1 1)))
    (is (= 0 (compare 1.1 1.1))))

  )

(deftest comparator-test
  (testing "comparator returns -1 if the predicate returns true"
    (let [cmp (comparator <)]
      (is (= -1 (cmp 1 2)))))

  (testing "comparator returns 1 if the predicate returns false"
    (let [cmp (comparator <)]
      (is (= 1 (cmp 2 1)))))

  (testing "comparator returns 0 if the predicate is neither true nor false"
    (let [cmp (comparator <)]
      (is (= 0 (cmp 1 1)))))

  )
