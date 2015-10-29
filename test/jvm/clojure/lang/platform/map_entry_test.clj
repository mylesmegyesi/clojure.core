(ns clojure.lang.platform.map-entry-test
  (:refer-clojure :only [let])
  (:require [clojure.test           :refer :all]
            [clojure.next           :refer :all]
            [clojure.lang.map-entry :refer [new-map-entry]])
  (:import  [java.util AbstractMap AbstractMap$SimpleEntry]))

(deftest map-entry-platform-test
  (testing "one map entry .equals another"
    (let [m1 (new-map-entry :k 1)
          m2 (new-map-entry :k 1)]
      (is (.equals m1 m2))))

  (testing "Map.Entry can be accessed with key and val"
    (let [m (AbstractMap$SimpleEntry. :k :v)]
      (is (= :k (key m)))
      (is (= :v (val m)))))

  (testing "map entry hash code"
    (is (= 961 (.hashCode (new-map-entry nil nil))))
    (is (= 994 (.hashCode (new-map-entry 1 2))))))
