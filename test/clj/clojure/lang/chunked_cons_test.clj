(ns clojure.lang.chunked-cons-test
  (:refer-clojure :only [defn- let loop when])
  (:require [clojure.test                 :refer :all]
            [clojure.next                 :refer :all]
            [clojure.lang.persistent-list :refer [EMPTY-LIST]]))

(defn- make-chunk [seqable]
  (let [cb (chunk-buffer (count seqable))]
    (loop [s (seq seqable)]
      (when s
        (chunk-append cb s)
        (recur (next s))))
    (chunk cb)))

(deftest chunked-cons-test
  (testing "chunk-cons returns the rest if the chunk is nil"
    (let [sentinel (vector)]
      (is (nil? (chunk-cons nil nil)))
      (is (= sentinel (chunk-cons nil sentinel)))))

  (testing "chunk-first returns the whole chunk"
    (let [chnk (make-chunk (vector :item))
          cc (chunk-cons chnk nil)]
      (is (identical? chnk (chunk-first cc)))))

  (testing "chunk-next returns the seq of the second argument"
    (is (nil? (chunk-next (chunk-cons (make-chunk (vector :item)) nil))))
    (is (= (vector 1 2 3)) (chunk-next (chunk-cons (make-chunk (vector :item)) (seq (vector 1 2 3))))))

  (testing "chunk-rest returns an empty list if the more seq is nil"
    (is (= EMPTY-LIST (chunk-rest (chunk-cons (make-chunk (vector :item)) nil)))))

  (testing "returns the count without a more seq"
    (is (= 3 (count (chunk-cons (make-chunk (vector 1 2 3)) nil)))))

  (testing "can set and get meta"
    (let [mta (array-map :so :meta)
          cc (chunk-cons (make-chunk (vector :foo)) nil)]
      (is (= mta (meta (with-meta cc mta))))))

  (testing "can conj an item"
    (let [cc (chunk-cons (make-chunk (vector :foo)) nil)
          c (conj cc :bar)]
      (is (= 2 (count c)))
      (is (= :bar (first c)))
      (is (= :foo (first (second c))))))

  (testing "empty returns the empty list"
    (let [cc (chunk-cons (make-chunk (vector :foo)) nil)]
      (is (= EMPTY-LIST (empty cc)))))

  (testing "first returns the first chunk element of the chunk"
    (let [cc (chunk-cons (make-chunk (vector :foo)) nil)]
      (is (= :foo (first (first cc))))))

  (testing "next returns a chunked-cons with the rest if the chunk has a size greater than 1"
    (let [cc (next (chunk-cons (make-chunk (vector :foo :bar)) nil))]
      (is (= 1 (count cc)))
      (is (= :bar (first (first cc))))))

  (testing "next returns the more seq when the chunk has one item"
    (let [cc (next (chunk-cons (make-chunk (vector :foo)) (conj EMPTY-LIST :bar)))]
      (is (= 1 (count cc)))
      (is (= :bar (first cc)))))

  (testing "next returns nil when there is no next"
    (let [cc (next (chunk-cons (make-chunk (vector :foo)) nil))]
      (is (nil? cc))))

  (testing "rest returns the empty list when there is no next"
    (let [cc (rest (chunk-cons (make-chunk (vector :foo)) nil))]
      (is (= EMPTY-LIST cc)))))

