(ns clojure.lang.size-test
  (:refer-clojure :only [let])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all])
  (:import [java.util HashMap LinkedList]))

(deftest platform-count-test
  (testing "returns the count of a string"
    (is (= 3 (count "foo"))))

  (testing "returns the count of a Collection"
    (let [ll (LinkedList.)]
      (.add ^LinkedList ll 1)
      (.add ^LinkedList ll 2)
      (.add ^LinkedList ll 3)
      (is (= 3 (count ll)))))

  (testing "returns the count of a Map"
    (let [hm (HashMap.)]
      (.put hm 1 1)
      (.put hm 2 2)
      (.put hm 3 3)
      (is (= 3 (count hm)))))

  (testing "returns the count of an Array"
    (let [arr (into-array (vector 1 2 3))]
      (is (= 3 (count arr)))))

  (testing "raises UnsupportedOperationException otherwise"
    (is
      (thrown-with-msg?
        UnsupportedOperationException
        #"count not supported on this type: Long"
        (count 1)))))

