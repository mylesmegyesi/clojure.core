(ns clojure.lang.platform.persistent-array-map-test
  (:refer-clojure :only [let])
  (:require [clojure.test                      :refer :all]
            [clojure.lang.comparison           :refer [=]]
            [clojure.lang.hash                 :refer [hash]]
            [clojure.lang.logical              :refer [not]]
            [clojure.lang.persistent-map       :refer [seq]]
            [clojure.lang.persistent-array-map :refer [array-map]]
            [clojure.lang.seq                  :refer [first next]])
  (:import [java.util NoSuchElementException]))

(deftest platform-equals-test
  (testing "one map is .equals to another"
    (let [m1 (array-map :k1 1)
          m2 (array-map :k1 1)]
      (is (.equals m1 m2)))))

(deftest platform-iterable-test
  (testing "hasNext when there is one item"
    (let [iterator (.iterator (array-map :k1 1))]
      (is (.hasNext iterator))))

  (testing "does not have next when there are no items"
    (let [iterator (.iterator (array-map))]
      (is (not (.hasNext iterator)))))

  (testing "throws an NoSuchElementException when there is no next iterable"
    (let [iterator (.iterator (array-map))]
      (is (thrown? NoSuchElementException (.next iterator)))))

  (testing "returns the next element when there is a next element"
    (let [m1 (array-map :k1 1)
          iterator (.iterator m1)]
      (is (= (first (seq m1))
             (.next iterator)))))

  (testing "return the next next element when there is a next next element"
    (let [m1 (array-map :k1 1 :k2 2)
          iterator (.iterator m1)]
      (.next iterator)
      (is (= (first (next (seq m1)))
             (.next iterator)))))

  (testing "throws an UnsupportedOperationException when trying to use remove"
    (let [iterator (.iterator (array-map))]
      (is (thrown? UnsupportedOperationException (.remove iterator))))))

(deftest platform-hash-code-test
  (testing "returns the hashCode"
    (let [m1 (array-map :k 1)]
      (is (= (hash m1) (.hashCode m1)))))
  )
