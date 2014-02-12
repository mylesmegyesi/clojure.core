(ns clojure.lang.lazy-seq-test
  (:refer-clojure :only [defn deftype let nil? nth rest zero?])
  (:require [clojure.test           :refer :all]
            [clojure.lang.protocols :refer [ISeq ISeqable]]
            [clojure.next           :refer :all]))

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
    (if (zero? (clojure.core/count -list))
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
    (is (nil? (next (next (next (lazy-seq test-seqable))))))))

  (testing "counting a lazy seq"
    (is (= 0 (count (lazy-seq))))
    (is (= 1 (count (lazy-seq (TestSeqable. '(1))))))))

(defn repeatedly-true []
  (TestSeqable. '(true (lazy-seq (repeatedly-true)))))

(deftest infinite-lazy-seq-test
  (testing "lazily traverses infinite seq"
    (= true (first (lazy-seq (repeatedly-true))))
    (= true (first (next (lazy-seq (repeatedly-true)))))))

(deftest lazy-seq-meta-test
  (testing "the initial meta is nil"
    (is (nil? (meta (lazy-seq)))))

  (testing "with-meta resets the meta value"
    (let [lazy-seq-with-meta (with-meta (lazy-seq) {:so :meta})]
      (is (= {:so :meta} (meta lazy-seq-with-meta))))))
