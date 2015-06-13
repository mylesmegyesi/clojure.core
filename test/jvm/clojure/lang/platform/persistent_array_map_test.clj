(ns clojure.lang.platform.persistent-array-map-test
  (:refer-clojure :only [defn- let list])
  (:require [clojure.test                              :refer :all]
            [clojure.lang.platform.persistent-map-test :refer [platform-map-test]]
            [clojure.support.test-seq                  :refer [test-seqable]]
            [clojure.next                              :refer :all])
  (:import [java.util AbstractMap AbstractMap$SimpleEntry]))

(defn- make-map-entry [k v]
  (AbstractMap$SimpleEntry. k v))

(deftest array-map-platform-test
  (platform-map-test array-map))

(deftest iterable-order-test
  (testing "return elements in the order that they were given"
    (let [m1 (array-map :k1 1 :k2 2)
          iterator (.iterator m1)]
      (.next iterator)
      (is (= (first (next (seq m1)))
             (.next iterator))))))

(deftest platform-transient-array-map-test
  (testing "conj! a map entry"
    (let [t (transient (array-map))]
      (conj! t (make-map-entry :k :v))
      (let [p (persistent! t)]
        (is (= 1 (count p)))
        (is (= :k (key (first p))))
        (is (= :v (val (first p)))))))

  (testing "conj! a seq of map entries"
    (let [t (transient (array-map))
          s (test-seqable (list (make-map-entry :k1 1) (make-map-entry :k2 2) (make-map-entry :k3 3)))]
      (conj! t s)
      (let [p (persistent! t)]
        (is (= 3 (count p)))
        (is (= :k1 (key (first p))))
        (is (= 1 (val (first p))))
        (is (= :k2 (key (first (rest p)))))
        (is (= 2 (val (first (rest p)))))
        (is (= :k3 (key (first (rest (rest p))))))
        (is (= 3 (val (first (rest (rest p))))))))))

