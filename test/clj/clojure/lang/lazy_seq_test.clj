(ns clojure.lang.lazy-seq-test
  (:refer-clojure :only [defn fn let >=])
  (:require [clojure.test                 :refer :all]
            [clojure.next                 :refer :all]
            [clojure.lang.persistent-list :refer [EMPTY-LIST]]
            [clojure.support.test-seq     :refer [test-seq test-seqable]]))

(deftest lazy-seq-test
  (testing "retrieving the first element"
    (let [test-seqable (test-seqable '(1))]
      (is (= 1 (first (lazy-seq test-seqable))))))

  (testing "retrieving the next sequences"
    (let [test-seqable (test-seqable '(1 2 3))
          s1 (seq test-seqable)
          s2 (next s1)
          s3 (next s2)]
    (is (= (first s1) (first (lazy-seq test-seqable))))
    (is (= (first s2) (first (next (lazy-seq test-seqable)))))
    (is (= (first s3) (first (next (next (lazy-seq test-seqable))))))
    (is (nil? (next (next (next (lazy-seq test-seqable))))))))

  (testing "counting a lazy seq"
    (is (= 0 (count (lazy-seq))))
    (is (= 1 (count (lazy-seq (test-seqable '(1)))))))

  (testing "rest of a lazy seq"
    (let [r (rest (lazy-seq (test-seqable '(1 2 3))))]
      (is (= 2 (first r)))
      (is (= 3 (second r)))))

  (testing "rest of an empty lazy seq"
    (is (= EMPTY-LIST (rest (lazy-seq))))))

(defn repeatedly-true []
  (test-seqable '(true (lazy-seq (repeatedly-true)))))

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

(deftest take-test
  (testing "take zero will an empty lazy-seq"
    (is (empty? (take 0 (test-seqable '(1 2 3))))))

  (testing "take some of a seq"
    (let [taken (take 2 (test-seqable '(1 2 3)))]
      (is (= 2 (count taken)))
      (is (= 1 (first taken)))
      (is (= 2 (second taken)))))

  (testing "take all of a seq"
    (let [taken (take 2 (test-seqable '(1 2)))]
      (is (= 2 (count taken)))
      (is (= 1 (first taken)))
      (is (= 2 (second taken)))))

  (testing "taking more elements than are in the seq returns the whole seq"
    (let [taken (take 3 (test-seqable '(1 2)))]
      (is (= 2 (count taken)))
      (is (= 1 (first taken)))
      (is (= 2 (second taken))))))

(deftest take-while-test
  (testing "take while false"
    (let [pred (fn [_] false)
          taken (take-while pred (test-seqable '(1 2 3)))]
      (is (empty? taken))))

  (testing "take some of a seq"
    (let [pred #(> 3 %)
          taken (take-while pred (test-seqable '(1 2 3)))]
      (is (= 2 (count taken)))
      (is (= 1 (first taken)))
      (is (= 2 (second taken)))))

  (testing "take all of a seq"
    (let [pred #(> 3 %)
          taken (take-while pred (test-seqable '(1 2)))]
      (is (= 2 (count taken)))
      (is (= 1 (first taken)))
      (is (= 2 (second taken))))))

(deftest repeat-test
  (testing "repeat a value"
    (let [repeater (repeat "inf")]
      (is (= "inf" (first repeater)))
      (is (= "inf" (nth repeater 42)))))

  (testing "take n of a repeated value"
    (let [n (repeat 2 "inf")]
      (is (= 2 (count n)))
      (is (= "inf" (first n)))
      (is (= "inf" (second n))))))

(deftest repeatedly-test
  (testing "repeat a function"
    (let [repeater (repeatedly #(rand))
          f (first repeater)
          s (second repeater)]
      (is (and (>= f 0) (> 1 f)))
      (is (and (>= s 0) (> 1 s)))))

  (testing "take n of a repeated function"
    (let [n (repeatedly 2 #(rand))
          f (first n)
          s (second n)]
      (is (= 2 (count n)))
      (is (and (>= f 0) (> 1 f)))
      (is (and (>= s 0) (> 1 s))))))

