(ns clojure.lang.persistent-vector-test
  (:refer-clojure :only [defn let nil? range apply])
  (:require [clojure.test                   :refer :all]
            [clojure.lang.persistent-vector :refer [vector]]
            [clojure.next                   :refer :all]))

(deftest vector-test
  (testing "returns the count"
    (let [zero (vector)
          one (vector 1)
          two (vector 1 2)
          more (apply vector (range 100))]
      (is (= 0 (count zero)))
      (is (= 1 (count one)))
      (is (= 2 (count two)))
      (is (= 100 (count more)))))

  (testing "returns the nth object"
    (let [new-vec (vector :a :b :c :d :e :f)]
      (is (= :a (nth new-vec 0)))
      (is (= :c (nth new-vec 2)))
      (is (= :f (nth new-vec 5)))))

  (testing "returns the nth object or not-found"
    (let [new-vec (vector :a :b :c :d :e :f)]
      (is (= :a (nth new-vec 0)))
      (is (= :c (nth new-vec 2)))
      (is (= :not-found (nth new-vec 8 :not-found)))))
)

(deftest vector-seq-test
  (testing "seq returns nil when empty"
    (is (nil? (seq (vector)))))

  (testing "returns the first item"
    (let [v-seq (seq (vector 3 2 1))]
      (is (= 3 (first v-seq)))))

  (testing "returns next until emtpy"
    (let [v1-seq (seq (vector 3 2 1))
          v2-seq (next v1-seq)
          v3-seq (next v2-seq)
          v4-seq (next v3-seq)]
      (is (= 2 (first v2-seq)))
      (is (= 1 (first v3-seq)))
      (is (nil? v4-seq))))

  (testing "returns the count"
    (let [v1-seq (seq (vector 3 2 1))
          v2-seq (next v1-seq)
          v3-seq (next v2-seq)]
      (is (= 3 (count v1-seq)))
      (is (= 2 (count v2-seq)))
      (is (= 1 (count v3-seq)))))

  (testing "is seqable"
    (let [v-seq (seq (vector :a :b :c))]
      (is (= :a (first (seq v-seq))))))
)
