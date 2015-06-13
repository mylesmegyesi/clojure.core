(ns clojure.lang.enumerable-test
  (:refer-clojure :only [defmacro list list* let])
  (:require [clojure.test                     :refer :all]
            [clojure.next                     :refer :all]
            [clojure.lang.enumerable          :refer :all]
            [clojure.support.test-seq         :refer [test-seq]]))

(deftest seq-iterator-test
  (testing "remove throws UnsupportedOperationException"
    (let [s (test-seq '(1 2 3))
          seq-iterator (new-seq-iterator s)]
      (is (thrown? UnsupportedOperationException
        (.remove seq-iterator)))))

  (testing "returns true while hasNext"
    (let [s (test-seq '(1 2 3))
          seq-iterator (new-seq-iterator s)]
      (is (true? (.hasNext seq-iterator)))
      (.next seq-iterator)
      (is (true? (.hasNext seq-iterator)))
      (.next seq-iterator)
      (is (true? (.hasNext seq-iterator)))
      (.next seq-iterator)
      (is (false? (.hasNext seq-iterator)))))

  (testing "returns the next item"
    (let [s (test-seq '(1 2 3))
          seq-iterator (new-seq-iterator s)]
      (is (= 1 (.next seq-iterator)))
      (is (= 2 (.next seq-iterator)))
      (is (= 3 (.next seq-iterator)))))

  (testing "next without a next throws a NoSuchElementException"
    (let [s (test-seq '(1))
          seq-iterator (new-seq-iterator s)]
      (.next seq-iterator)
      (is (thrown? java.util.NoSuchElementException
        (.next seq-iterator)))))

  )
