(ns clojure.lang.platform.persistent-array-map-test
  (:refer-clojure :only [let])
  (:require [clojure.test                              :refer :all]
            [clojure.lang.operators                    :refer [=]]
            [clojure.lang.platform.persistent-map-test :refer [platform-map-test]]
            [clojure.lang.persistent-array-map         :refer [array-map]]
            [clojure.lang.seq                          :refer [first next seq]]))

(deftest array-map-platform-test
  (platform-map-test array-map))

(deftest iterable-order-test
  (testing "return elements in the order that they were given"
    (let [m1 (array-map :k1 1 :k2 2)
          iterator (.iterator m1)]
      (.next iterator)
      (is (= (first (next (seq m1)))
             (.next iterator))))))
