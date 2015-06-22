(ns clojure.lang.map-entry-test
  (:refer-clojure :only [let])
  (:require [clojure.test                         :refer :all]
            [clojure.lang.map-entry               :refer [new-map-entry]]
            [clojure.support.exception-assertions :refer [out-of-bounds-exception-is-thrown?]]
            [clojure.next                         :refer :all]))

(deftest map-entry-test
  (testing "it generates a new map entry with a key and value"
    (let [me1 (new-map-entry :key :val)]
      (is (and (= (key me1) :key)
               (= (val me1) :val)))))

  (testing "equal with the same key and val"
    (let [me1 (new-map-entry :k1 :v1)
          me2 (new-map-entry :k1 :v1)]
      (is (= me1 me2))))

  (testing "not equal with a different key"
    (let [me1 (new-map-entry :k1 :v1)
          me2 (new-map-entry :k2 :v1)]
      (is (not= me1 me2))))

  (testing "not equal with a different value"
    (let [me1 (new-map-entry :k1 :v1)
          me2 (new-map-entry :k1 :v2)]
      (is (not= me1 me2))))

  (testing "not equal when the rhs is not equal"
    (let [me1 (new-map-entry :k1 :v1)]
      (is (not= me1 1))))

  (testing "the count is 2"
    (is (= 2 (count (new-map-entry :k :v)))))

  (testing "nth of 0 is the key"
    (is (= :k (nth (new-map-entry :k :v) 0))))

  (testing "nth of 1 is the val"
    (is (= :v (nth (new-map-entry :k :v) 1))))

  (testing "nth greater than 1 with no default throw an exception"
    (let [me (new-map-entry :k :v)]
      (out-of-bounds-exception-is-thrown?
        #".*"
        (nth me 2))))

  (testing "nth greater than 1 with a default returns the default"
    (is (= :default (nth (new-map-entry :k :v) 2 :default))))

  (testing "get of 0 is the key"
    (is (= :k (get (new-map-entry :k :v) 0))))

  (testing "get of 1 is the val"
    (is (= :v (get (new-map-entry :k :v) 1))))

  (testing "get greater than 1 with no default returns nil"
    (is (nil? (get (new-map-entry :k :v) 2))))

  (testing "get greater than 1 with a default returns the default"
    (is (= :default (get (new-map-entry :k :v) 2 :default))))

  (testing "cons an element"
    (is
      (= :third
         (first (cons :third (new-map-entry :k :v))))))

  (testing "empty returns nil"
    (is (nil? (empty (new-map-entry :k :v)))))

  )

(deftest map-entry-seq-test
  (testing "first of the seq is the key"
    (is (= :k (first (seq (new-map-entry :k :v))))))

  (testing "second of the seq is the value"
    (is (= :v (second (seq (new-map-entry :k :v))))))

  )
