(ns clojure.next-test
  (:refer-clojure :only [apply let map])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all]))

(deftest constantly-test
  (testing "returns the return value"
    (is (= :val ((constantly :val)))))

  (testing "takes variable args and still returns the return value"
    (let [constant-fn (constantly :val)]
      (map #(is (= :val (apply constant-fn %)))
        [[] [1] [1 2] [1 2 3]]))))
