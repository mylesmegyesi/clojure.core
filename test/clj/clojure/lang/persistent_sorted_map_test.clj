(ns clojure.lang.persistent-sorted-map-test
  (:refer-clojure :only [defmacro deftype defn let list list* nil? re-pattern])
  (:require [clojure.test                       :refer :all]
            [clojure.lang.persistent-map-test   :refer [map-test]]
            [clojure.lang.persistent-sorted-map :refer [sorted-map]]

            [clojure.lang.counted               :refer [count]]
            [clojure.lang.ihash                 :refer [IHash]]
            [clojure.lang.hash                  :refer [hash]]
            [clojure.lang.lookup                :refer [contains? get]]
            [clojure.lang.map-entry             :refer [key val]]
            [clojure.lang.operators             :refer [not not= =]]
            [clojure.lang.persistent-map        :refer [assoc dissoc]]
            [clojure.lang.persistent-sorted-map :refer :all]
            [clojure.lang.platform.exceptions   :refer [argument-error]]
            [clojure.lang.platform.object       :refer [identical?]]
            [clojure.lang.seq                   :refer [seq first next]]
            [clojure.lang.show                  :refer [str]]
            ))

; uncomment this when the rest of sorted map is implemented
;(deftest sorted-map-test
;  (map-test "PersistentSortedMap" sorted-map))

(defmacro argument-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? argument-error msg body)))

(deftest persistent-sorted-map-test
  (testing "creates a map with 0 items"
    (is (= 0 (count (sorted-map)))))

  (testing "creates a map with initial values"
    (let [m1 (sorted-map :k1 1)
          m2 (sorted-map :k1 1 :k2 2)]
      (is (= 1 (get m1 :k1)))
      (is (= nil (get m1 :k2)))
      (is (= 1 (count m1)))
      (is (= 1 (get m2 :k1)))
      (is (= 2 (get m2 :k2)))
      (is (= 2 (count m2)))))

  (testing "throws an exception if there are not an even number of arguements"
    (argument-error-is-thrown?
      #"PersistentSortedMap can only be created with even number of arguments: 3 arguments given"
      (sorted-map :k1 1 :k2)))

  (testing "associates a key to a value"
    (let [m1 (sorted-map)
          m2 (assoc m1 :key 1)]
      (is (not (identical? m1 m2)))
      (is (not= m1 m2))
      (is (nil? (get m1 :key)))
      (is (= 1 (get m2 :key)))
      (is (= 0 (count m1)))
      (is (= 1 (count m2)))))

  (testing "associates a key to a value when the key already exists"
    (let [m1 (sorted-map)
          m2 (assoc m1 :key 1)
          m3 (assoc m2 :key 2)]
      (is (not (identical? m2 m3)))
      (is (not= m2 m3))
      (is (nil? (get m1 :key)))
      (is (= 1 (get m2 :key)))
      (is (= 2 (get m3 :key)))
      (is (= 0 (count m1)))
      (is (= 1 (count m2)))
      (is (= 1 (count m3)))))

  (testing "associates a key to a value when the key already exists and the value is the same"
    (let [m1 (sorted-map)
          m2 (assoc m1 :key 1)
          m3 (assoc m2 :key 1)]
      (is (identical? m2 m3))))

  (testing "associates many keys to their values"
    (let [m1 (sorted-map :k1 1)
          m2 (assoc m1 :k2 2 :k3 3 :k1 4)]
      (is (= 4 (get m2 :k1)))
      (is (= 2 (get m2 :k2)))
      (is (= 3 (get m2 :k3)))))

  (testing "returns a provided not-found value when using get"
    (let [m1 (sorted-map)]
      (is (= "not found" (get m1 :not-a-key "not found")))))

  (testing "contains? a key if the key is present in the map"
    (let [m1 (sorted-map :k1 1)]
      (is (contains? m1 :k1))))

  (testing "does not contains? a key if the key is not present in the map"
    (let [m1 (sorted-map :k1 1)]
      (is (not (contains? m1 :k2)))))

  (testing "does not contains? a key if the key is present AS A VALUE in the map"
    (let [m1 (sorted-map :k1 :v1)]
      (is (not (contains? m1 :v1)))))

  )
