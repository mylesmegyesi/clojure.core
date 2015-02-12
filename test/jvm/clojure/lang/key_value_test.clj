(ns clojure.lang.key-value-test
  (:refer-clojure :only [let])
  (:require [clojure.test           :refer :all]
            [clojure.next           :refer :all]
            [clojure.lang.map-entry :refer [new-map-entry]]))

(deftest java.util.Map$Entry-test
  (testing "retrieval of a key"
    (let [map-entry (new-map-entry :k :v)]
      (is (= :k (.getKey map-entry)))))

  (testing "retrieval of a value"
    (let [map-entry (new-map-entry :k :v)]
      (is (= :v (.getValue map-entry))))))

