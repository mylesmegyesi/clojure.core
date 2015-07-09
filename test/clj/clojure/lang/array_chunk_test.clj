(ns clojure.lang.array-chunk-test
  (:refer-clojure :only [let ->])
  (:require [clojure.test                         :refer :all]
            [clojure.next                         :refer :all]
            [clojure.lang.array-chunk             :refer [make-array-chunk]]
            [clojure.lang.protocols               :refer [-drop-first]]
            [clojure.support.exception-assertions :refer [illegal-state-error-is-thrown?]]
            [clojure.support.test-seq             :refer [test-seq]]))

(deftest array-chunk-test
  (testing "count is the length of the array"
    (let [arr (into-array (test-seq '(1 2 3)))
          arr-chunk (make-array-chunk arr)]
      (is (= 3 (count arr-chunk)))))

  (testing "count respects the off and end"
    (let [arr (into-array (test-seq '(1 2 3)))
          arr-chunk (make-array-chunk arr 2 3)]
      (is (= 1 (count arr-chunk)))))

  (testing "nth of an array chunk"
    (let [arr (into-array (test-seq '(1 2 3)))
          arr-chunk (make-array-chunk arr)]
      (is (= 2 (nth arr-chunk 1)))))

  (testing "nth when not in bounds"
    (let [arr (into-array (test-seq '(1 2 3)))
          arr-chunk (make-array-chunk arr)]
      (is (= :not-found (nth arr-chunk 42 :not-found)))))

  (testing "drop-first"
    (let [arr (into-array (test-seq '(1 2 3)))
          arr-chunk (->
                      (make-array-chunk arr)
                      (-drop-first))]
      (is (= 2 (nth arr-chunk 0))))))

