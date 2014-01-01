(ns clojure.lang.persistent-array-map-test
  (:refer-clojure :only [let nil?])
  (:require [clojure.test                      :refer :all]
            [clojure.lang.associative          :refer [assoc]]
            [clojure.lang.counted              :refer [count]]
            [clojure.lang.equivalence          :refer [= not not=]]
            [clojure.lang.lookup               :refer [get]]
            [clojure.lang.persistent-array-map :refer :all]
            [clojure.lang.platform.object      :refer [identical?]]))

(deftest map-test

  (testing "creates a map with 0 items"
    (is (= 0 (count (array-map)))))

  (testing "associates a key to a value"
    (let [m1 (array-map)
          m2 (assoc m1 :key 1)]
      (is (not (identical? m1 m2)))
      (is (not= m1 m2))
      (is (nil? (get m1 :key)))
      (is (= 1 (get m2 :key)))
      (is (= 0 (count m1)))
      (is (= 1 (count m2)))))

  )
