(ns clojure.lang.persistent-list-test
  (:refer-clojure :only [let])
  (:require [clojure.test                 :refer :all]
            [clojure.next                 :refer :all]
            [clojure.lang.persistent-list :refer :all]))

(deftest list-test
  (testing "creates a list"
    (is (= 3 (count (list 1 2 3)))))

  (testing "meta"
    (let [meta-list (with-meta (list "a" "b") {:my :meta})]
      (is (= :meta (:my (meta meta-list))))))

  (testing "empty list"
    (let [new-list (with-meta (list :a :b :c) {:this :that})
          empty-list (empty new-list)]
      (is (= 0 (count empty-list)))
      (is (= :that (:this (meta empty-list))))))
)
