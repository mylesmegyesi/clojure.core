(ns clojure.lang.persistent-list-test
  (:refer-clojure :only [let apply range])
  (:require [clojure.test :refer :all]
            [clojure.lang.persistent-list :refer [list]]
            [clojure.next :refer :all]))

(deftest list-test
  (testing "count"
    (is (= 3 (count (list 1 2 3)))))

  (testing "first"
    (is (= 1 (first (list 1 2 3)))))

  (testing "next"
    (is (= (list 2 3) (next (list 1 2 3)))))

  (testing "meta"
    (let [meta-list (with-meta (list "a" "b") {:my :meta})]
      (is (= :meta (:my (meta meta-list))))))

  (testing "empty"
    (let [new-list (with-meta (list :a :b :c) {:this :that})
          empty-list (empty new-list)]
      (is (= 0 (count empty-list)))
      (is (= :that (:this (meta empty-list))))))

  (testing "peek"
    (let [new-list (apply list (range 50))]
      (is (= 0 (peek new-list)))))

  (testing "pop"
    (let [new-list (list "one" "two" "three")]
      (is (= (list "two" "three") (pop new-list)))))
)
