(ns clojure.lang.platform.persistent-map-test
  (:refer-clojure :only [defn let])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all])
  (:import [java.util NoSuchElementException]))

(defn platform-equals-test [constructor]
  (testing "one map is .equals to another"
    (let [m1 (constructor :k1 1)
          m2 (constructor :k1 1)]
      (is (.equals m1 m2)))))

(defn platform-iterable-test [constructor]
  (testing "hasNext when there is one item"
    (let [iterator (.iterator (constructor :k1 1))]
      (is (.hasNext iterator))))

  (testing "does not have next when there are no items"
    (let [iterator (.iterator (constructor))]
      (is (not (.hasNext iterator)))))

  (testing "throws an NoSuchElementException when there is no next iterable"
    (let [iterator (.iterator (constructor))]
      (is (thrown? NoSuchElementException (.next iterator)))))

  (testing "returns the next element when there is a next element"
    (let [m1 (constructor :k1 1)
          iterator (.iterator m1)]
      (is (= (first (seq m1))
             (.next iterator)))))

  (testing "throws an UnsupportedOperationException when trying to use remove"
    (let [iterator (.iterator (constructor))]
      (is (thrown? UnsupportedOperationException (.remove iterator))))))

(defn platform-hash-code-test [constructor]
  (testing "returns the hashCode"
    (let [m1 (constructor :k 1)]
      (is (= (hash m1) (.hashCode m1))))))

(defn platform-map-test [constructor]
  (platform-equals-test constructor)
  (platform-iterable-test constructor)
  (platform-hash-code-test constructor))
