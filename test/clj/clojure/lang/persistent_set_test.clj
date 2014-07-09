(ns clojure.lang.persistent-set-test
  (:refer-clojure :only [even? let type map])
  (:require [clojure.test                       :refer :all]
            [clojure.lang.persistent-set        :refer :all]
            [clojure.next                       :refer [= contains?]]))

(deftest persistent-set-test
  (testing "set produces a new hash-set from a seq"
    (let [s1 (set '(1 2 3))]
      (is 'PersistentHashSet (type s1))
      (is (contains? s1 1))
      (is (contains? s1 2))
      (is (contains? s1 3))))

  (testing "select all elements for which the predicate is true"
    (let [s (set '(1 2 3 4 5))]
      (is (= (set '(2 4)) (select even? s)))))

  )

