(ns clojure.lang.persistent-queue-test
  (:refer-clojure :only [let nil?])
  (:require [clojure.test                  :refer :all]
            [clojure.lang.persistent-queue :refer :all]
            [clojure.next                  :refer :all]))

(deftest persistent-queue-test
  (testing "conj and peek"
    (is (= nil (peek EMPTY-QUEUE)))
    (is (= 1 (peek (conj EMPTY-QUEUE 1))))
    (is (= 2 (peek (conj (conj EMPTY-QUEUE 2) 1)))))

  (testing "pop and count"
    (is (= 0 (count EMPTY-QUEUE)))
    (is (= 1 (count (conj EMPTY-QUEUE 1))))
    (is (= 2 (count (conj (conj EMPTY-QUEUE 2) 1))))
    (is (= 1 (count (pop (conj (conj EMPTY-QUEUE 2) 1)))))
    (is (= 0 (count (pop (conj EMPTY-QUEUE 1)))))
    (is (= 0 (count (pop EMPTY-QUEUE)))))

  (testing "empty"
    (is (empty? EMPTY-QUEUE))
    (is (not (empty? (conj EMPTY-QUEUE :something)))))

  (testing "includes an item"
    (let [single-item-queue    (conj EMPTY-QUEUE :anything)
          multi-item-queue     (conj (conj (conj EMPTY-QUEUE :something) :anything) :at-all)
          non-containing-queue (conj EMPTY-QUEUE :thing)]
      (is (contains? single-item-queue :anything))
      (is (contains? multi-item-queue :anything))
      (is (not (contains? non-containing-queue :anything)))))

  (testing "meta"
    (let [meta-queue (with-meta EMPTY-QUEUE {:so :meta})]
      (is (nil? (meta EMPTY-QUEUE)))
      (is (= {:so :meta} (meta meta-queue)))))

  )

(deftest persistent-queue-seq-test
  (testing "seq returns nil when empty"
    (is (nil? (seq EMPTY-QUEUE))))

  (testing "returns the first item"
    (let [queue (conj (conj EMPTY-QUEUE :first) :second)]
      (is (= :first (first queue)))))

  (testing "returns next until empty"
    (let [q1-seq (seq (conj (conj (conj EMPTY-QUEUE 1) 2) 3))
          q2-seq (next q1-seq)
          q3-seq (next q2-seq)
          q4-seq (next q3-seq)]
      (is (= 2 (first q2-seq)))
      (is (= 3 (first q3-seq)))
      (is (nil? q4-seq))))

  (testing "returns rest until empty"
    (let [q1-seq (seq (conj (conj (conj EMPTY-QUEUE 1) 2) 3))
          q2-seq (rest q1-seq)
          q3-seq (rest q2-seq)
          q4-seq (rest q3-seq)]
      (is (= 2 (first q2-seq)))
      (is (= 3 (first q3-seq)))
      (is (empty? q4-seq))))

  (testing "returns the count"
    (let [q1-seq (seq (conj (conj (conj EMPTY-QUEUE 1) 2) 3))
          q2-seq (next q1-seq)
          q3-seq (next q2-seq)]
      (is (= 3 (count q1-seq)))
      (is (= 2 (count q2-seq)))
      (is (= 1 (count q3-seq)))))

  (testing "is seqable"
    (let [q-seq (seq (conj (conj (conj EMPTY-QUEUE 1) 2) 3))]
      (is (= 1 (first (seq q-seq))))))

  (testing "meta"
    (let [meta-queue-seq (with-meta (seq (conj EMPTY-QUEUE 1)) {:so :meta})]
      (is (nil? (meta (seq (conj EMPTY-QUEUE 1)))))
      (is (= {:so :meta} (meta meta-queue-seq)))))

  )
