(ns clojure.lang.lazy-seq-test
  (:refer-clojure :only [count defn deftype let nil? nth rest zero?])
  (:require [clojure.test           :refer :all]
            [clojure.lang.protocols :refer [ISeq ISeqable]]
            [clojure.next           :refer :all :exclude [count]]))

(deftype TestSeq [-list]
  ISeqable
  (-seq [this] this)

  ISeq
  (-first [this]
    (nth -list 0))

  (-next [this]
    (if (= '() (rest -list))
      nil
      (TestSeq. (rest -list)))))

(deftype TestSeqable [-list]
  ISeqable
  (-seq [this]
    (if (zero? (count -list))
      nil
      (TestSeq. -list))))

(deftest lazy-seq-test
  (testing "retrieving the first element"
    (let [test-seqable (TestSeqable. '(1))]
      (is (= 1 (first (lazy-seq test-seqable))))))

  (testing "retrieving the next sequences"
    (let [test-seqable (TestSeqable. '(1 2 3))
          s1 (seq test-seqable)
          s2 (next s1)
          s3 (next s2)]
    (is (= (first s1) (first (lazy-seq test-seqable))))
    (is (= (first s2) (first (next (lazy-seq test-seqable)))))
    (is (= (first s3) (first (next (next (lazy-seq test-seqable))))))
    (is (nil? (next (next (next (lazy-seq test-seqable)))))))))

(defn repeatedly-true []
  (TestSeqable. '(true (lazy-seq (repeatedly-true)))))

(deftest infinite-lazy-seq-test
  (testing "lazily traverses infinite seq"
    (= true (first (lazy-seq (repeatedly-true))))
    (= true (first (next (lazy-seq (repeatedly-true)))))))
