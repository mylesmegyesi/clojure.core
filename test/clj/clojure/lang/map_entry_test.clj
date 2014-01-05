(ns clojure.lang.map-entry-test
  (:refer-clojure :only [and let])
  (:require [clojure.test            :refer :all]
            [clojure.lang.comparison :refer [not= =]]
            [clojure.lang.map-entry  :refer [make-map-entry key val]]))

(deftest map-entry-test
  (testing "it generates a new map entry with a key and value"
    (let [me1 (make-map-entry :key :val)]
      (is (and (= (key me1) :key)
               (= (val me1) :val)))))

  (testing "equal with the same key and val"
    (let [me1 (make-map-entry :k1 :v1)
          me2 (make-map-entry :k1 :v1)]
      (is (= me1 me2))))

  (testing "not equal with a different key"
    (let [me1 (make-map-entry :k1 :v1)
          me2 (make-map-entry :k2 :v1)]
      (is (not= me1 me2))))

  (testing "not equal with a different value"
    (let [me1 (make-map-entry :k1 :v1)
          me2 (make-map-entry :k1 :v2)]
      (is (not= me1 me2))))

  (testing "not equal when the rhs is not equal"
    (let [me1 (make-map-entry :k1 :v1)]
      (is (not= me1 1)))))
