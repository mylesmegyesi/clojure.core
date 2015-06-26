(ns clojure.lang.platform.persistent-map-test
  (:refer-clojure :only [defn defn- let list ->])
  (:require [clojure.test             :refer :all]
            [clojure.support.test-seq :refer [test-seqable]]
            [clojure.next             :refer :all])
  (:import [java.util AbstractMap AbstractMap$SimpleEntry
                      NoSuchElementException]))

(defn- make-map-entry [k v]
  (AbstractMap$SimpleEntry. k v))

(defn- platform-equals-test [constructor]
  (testing "one map is .equals to another"
    (let [m1 (constructor :k1 1)
          m2 (constructor :k1 1)]
      (is (.equals m1 m2)))))

(defn- platform-iterable-test [constructor]
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

(defn- platform-hash-code-test [constructor]
  (testing "returns the hashCode"
    (let [m1 (constructor :k 1)]
      (is (= (hash m1) (.hashCode m1))))))

(defn- platform-conj-test [constructor]
  (testing "conj a map entry"
    (let [m (conj (constructor) (make-map-entry :k :v))]
      (is (= 1 (count m)))
      (is (= :k (key (first m))))
      (is (= :v (val (first m))))))

  (testing "conj a seq of map entries"
    (let [sq (test-seqable (list (make-map-entry :k1 1) (make-map-entry :k2 2)))
          m (-> (constructor) (conj sq))
          ks (keys m)
          vs (vals m)]
      (is (= 2 (count m))))))

(defn platform-map-test [constructor]
  (platform-equals-test constructor)
  (platform-iterable-test constructor)
  (platform-hash-code-test constructor)
  (platform-conj-test constructor))

