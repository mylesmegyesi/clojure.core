(ns clojure.lang.comparison-test
  (:refer-clojure :only [deftype let <])
  (:require [clojure.test             :refer :all]
            [clojure.lang.test-helper :refer [new-ten-comparator]]
            [clojure.lang.comparison  :refer [compare comparator]]
            [clojure.lang.operators   :refer [=]]))

(deftest compare-test
  (testing "compare uses the internal compare method of the first argument"
    (is (= 10 (compare (new-ten-comparator) :foo))))

  (testing "returns 0 if the two instances are the same"
    (let [instance (new-ten-comparator)]
      (is (= 0 (compare instance instance)))
      (is (= 0 (compare nil nil)))))

  (testing "returns -1 if the first element is nil"
    (is (= -1 (compare nil (new-ten-comparator)))))

  (testing "returns 1 if the second element is nil"
    (is (= 1 (compare (new-ten-comparator) nil))))

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
  (testing "comparator returns 1 if the predicate returns true with argument x y"
           (let [cmp (comparator <)]
             (is (= -1 (cmp 1 2)))))

  (testing "comparator returns -1 if the predicate returns true with arguments y x"
           (let [cmp (comparator <)]
             (is (= 1 (cmp 2 1)))))

  (testing "comparator returns 0 otherwise"
           (let [cmp (comparator <)]
             (is (= 0 (cmp 1 1))))))
