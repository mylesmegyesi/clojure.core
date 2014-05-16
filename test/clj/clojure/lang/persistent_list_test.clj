(ns clojure.lang.persistent-list-test
  (:refer-clojure :only [let])
  (:require [clojure.test                 :refer :all]
            [clojure.next                 :refer :all]
            [clojure.lang.persistent-list :refer :all]))

(deftest list-test
  (testing "creates a list"
    (is (= 3 (count (list 1 2 3)))))
)
