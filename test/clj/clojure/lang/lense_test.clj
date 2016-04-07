(ns clojure.lang.lense-test
  (:refer-clojure :only [let])
  (:require [clojure.test                 :refer :all]
            [clojure.next                 :refer :all]))

(deftest get-in-test
  (testing "an empty keys seq returns the original collection"
    (let [m (hash-map "foo" "bar")]
      (is (= m (get-in m (vector))))))

  (testing "get a value from present keys"
    (let [m (hash-map "foo" (hash-map "bar" "baz"))]
      (is (= "baz" (get-in m (vector "foo" "bar"))))))

  (testing "get nil without present keys"
    (is (nil? (get-in (hash-map "foo" "bar") (vector "baz"))))
    (is (nil? (get-in (hash-map "foo" "bar") (vector "baz" "bad")))))

  (testing "get-in with a not-found value"
    (is (= "not-found" (get-in (hash-map "foo" "bar") (vector "baz") "not-found")))
    (is (= "not-found" (get-in (hash-map "foo" "bar") (vector "baz" "bad") "not-found")))))

(deftest assoc-in-test
  (testing "an empty keys seq will assoc the value to the nil key"
    (let [m (hash-map)
          assoc-m (assoc-in m (vector) "val")]
      (is (= "val" (get assoc-m nil)))))

  (testing "assoc-in new nested structure"
    (let [m (hash-map "foo" "bar")
          assoc-m (assoc-in m (vector "baz" "bad") "val")]
      (is (= "val" (get-in assoc-m (vector "baz" "bad"))))))

  (testing "assoc-in an exiting nested structure"
    (let [m (hash-map "foo" (hash-map "bar" "baz"))
          assoc-m (assoc-in m (vector "foo" "bar") "val")]
      (is (= "val" (get-in assoc-m (vector "foo" "bar")))))))

