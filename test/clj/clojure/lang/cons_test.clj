(ns clojure.lang.cons-test
  (:refer-clojure :only [let list? nil?])
  (:require [clojure.test             :refer :all]
            [clojure.next             :refer :all]
            [clojure.support.test-seq :refer [test-seq test-seqable]]))

(deftest cons-test
  (testing "cons with a nil collection returns a list with a single element"
    (let [cons-nil (cons :elem nil)]
      (is (list? cons-nil))
      (is (= :elem (first cons-nil)))
      (is (nil? (next cons-nil)))))

  (testing "first of cons is the element"
    (let [cns (cons 1 (test-seqable '(2)))]
      (is (= 1 (first cns)))))

  (testing "next of cons is equivalent the seq of the collection"
    (let [test-seq (test-seq '(2))
          cns (cons 1 test-seq)]
      (is (== test-seq (next cns)))))

  (testing "count is the count of the seq plus 1"
    (is (= 2 (count (cons 1 (test-seqable '(2))))))
    (is (= 3 (count (cons 1 (test-seqable '(2 3))))))))

(deftest cons-meta-test
  (testing "meta is nil when instantiated"
    (let [cns (cons 1 (test-seqable '(2)))]
      (is (= {} (meta cns)))))

  (testing "meta can be set"
    (let [cns (cons 1 (test-seqable '(2)))
          mta-cns (with-meta cns {:foo :bar})]
      (is (= {:foo :bar} (meta mta-cns))))))
