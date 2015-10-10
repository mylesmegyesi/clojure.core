(ns clojure.lang.regex-test
  (:refer-clojure :only [let])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all]))

(deftest re-groups-test
  (testing "find groups when creating a matcher from a pattern"
    (is (= (seq (vector "12/09/1988" "12/09/1988" "12" "09" "1988"))
           (seq (re-find
             (re-matcher
               (re-pattern "(([0-9]{2})/([0-9]{2})/([0-9]{4}))") "12/09/1988")))))))

(deftest re-find-test
  (testing "find matches when creating a matcher from a pattern"
    (is (= "123"
           (re-find
              (re-matcher
                (re-pattern "\\d+") "foo123bar")))))

  (testing "find matches when using a pattern and a string"
    (is (= "123"
           (re-find
             (re-pattern "\\d+") "foo123bar")))))

(deftest re-seq-test
  (testing "finding no matches returns an empty seq"
    (is (empty? (re-seq (re-pattern "\\d+") "foobar"))))

  (testing "finding a single match"
    (let [s (re-seq (re-pattern "\\d+") "foo123bar")]
      (is (= 1 (count s)))
      (is (= "123" (first s)))))

  (testing "finding multiple matches"
    (let [s (re-seq (re-pattern "\\d+") "foo123bar456")]
      (is (= 2 (count s)))
      (is (= "123" (first s)))
      (is (= "456" (second s))))))

