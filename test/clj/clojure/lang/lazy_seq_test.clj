(ns clojure.lang.lazy-seq-test
  (:refer-clojure :only [defn fn let])
  (:require [clojure.test                 :refer :all]
            [clojure.next                 :refer :all]
            [clojure.lang.persistent-list :refer [EMPTY-LIST list]]))

(deftest lazy-seq-test
  (testing "retrieving the first element"
    (let [test-seqable (list 1)]
      (is (= 1 (first (lazy-seq test-seqable))))))

  (testing "retrieving the next sequences"
    (let [test-seqable (list 1 2 3)
          s1 (seq test-seqable)
          s2 (next s1)
          s3 (next s2)]
    (is (= (first s1) (first (lazy-seq test-seqable))))
    (is (= (first s2) (first (next (lazy-seq test-seqable)))))
    (is (= (first s3) (first (next (next (lazy-seq test-seqable))))))
    (is (nil? (next (next (next (lazy-seq test-seqable))))))))

  (testing "counting a lazy seq"
    (is (= 0 (count (lazy-seq))))
    (is (= 1 (count (lazy-seq (list 1))))))

  (testing "rest of a lazy seq"
    (let [r (rest (lazy-seq (list 1 2 3)))]
      (is (= 2 (first r)))
      (is (= 3 (second r)))))

  (testing "rest of an empty lazy seq"
    (is (= EMPTY-LIST (rest (lazy-seq)))))

  (testing "realizing a lazy seq"
    (let [l (lazy-seq (list 1 2 3))]
      (is (not (realized? l)))
      (seq l)
      (is (realized? l)))))

(defn repeatedly-true []
  (list true (lazy-seq (repeatedly-true))))

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
    (is (empty? (take 0 (list 1 2 3)))))

  (testing "take some of a seq"
    (let [taken (take 2 (list 1 2 3))]
      (is (= 2 (count taken)))
      (is (= 1 (first taken)))
      (is (= 2 (second taken)))))

  (testing "take all of a seq"
    (let [taken (take 2 (list 1 2))]
      (is (= 2 (count taken)))
      (is (= 1 (first taken)))
      (is (= 2 (second taken)))))

  (testing "taking more elements than are in the seq returns the whole seq"
    (let [taken (take 3 (list 1 2))]
      (is (= 2 (count taken)))
      (is (= 1 (first taken)))
      (is (= 2 (second taken))))))

(deftest take-while-test
  (testing "take while false"
    (let [pred (fn [_] false)
          taken (take-while pred (list 1 2 3))]
      (is (empty? taken))))

  (testing "take some of a seq"
    (let [pred #(> 3 %)
          taken (take-while pred (list 1 2 3))]
      (is (= 2 (count taken)))
      (is (= 1 (first taken)))
      (is (= 2 (second taken)))))

  (testing "take all of a seq"
    (let [pred #(> 3 %)
          taken (take-while pred (list 1 2))]
      (is (= 2 (count taken)))
      (is (= 1 (first taken)))
      (is (= 2 (second taken))))))

(deftest drop-while-test
  (testing "drop while false"
    (let [pred (fn [_] false)
          coll (drop-while pred (list 1 2))]
      (is (= 2 (count coll)))
      (is (= 1 (first coll)))
      (is (= 2 (second coll)))))

  (testing "drop some of a seq"
    (let [pred #(> 3 %)
          coll (drop-while pred (list 1 2 3))]
      (is (= 1 (count coll)))
      (is (= 3 (first coll)))))

  (testing "drop all of a seq"
    (let [pred #(> 3 %)
          coll (drop-while pred (list 1 2))]
      (is (empty? coll)))))

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

(deftest distinct-test
  (testing "distinct without repeated elements is a seq of the elements"
    (let [v (vector 1 2 3)
          d (distinct v)]
      (is (= 3 (count d)))
      (is (= 1 (nth d 0)))
      (is (= 2 (nth d 1)))
      (is (= 3 (nth d 2)))))

  (testing "distinct values in collection"
    (let [v (vector 1 2 2 3)
          d (distinct v)]
      (is (= 3 (count d)))
      (is (= 1 (nth d 0)))
      (is (= 2 (nth d 1)))
      (is (= 3 (nth d 2))))))

(deftest concat-test
  (testing "concat with no args creates an empty lazy seq"
    (let [s (concat)]
      (is (not (realized? s)))
      (is (empty? s))
      (is (realized? s))))

  (testing "concat arbitrary seqables"
    (let [s (concat (vector 1 2) (list 3 4))]
      (is (= 4 (count s)))
      (is (= 1 (first s)))
      (is (= 2 (first (next s))))
      (is (= 3 (first (next (next s)))))
      (is (= 4 (first (next (next (next s))))))
      (is (nil? (first (next (next (next (next s)))))))))
  )

