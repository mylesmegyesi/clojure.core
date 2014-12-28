(ns clojure.lang.persistent-sorted-map-test
  (:refer-clojure :only [conj defn- distinct let loop nil? rand-nth remove reduce repeatedly sort >])
  (:require [clojure.test                       :refer :all]
            [clojure.lang.persistent-map-test   :refer [map-test]]
            [clojure.lang.persistent-list       :refer [EMPTY-LIST]]
            [clojure.lang.persistent-sorted-map :refer :all]
            [clojure.lang.exceptions            :refer [argument-error]]
            [clojure.next                       :refer :all :exclude [reduce conj]]))

(deftest sorted-map-test
  (map-test "PersistentTreeMap" sorted-map))

(deftest sorted-map-rebalance-test
  (testing "assoc to an existing key"
    (let [m1 (sorted-map :k1 :v1)
          m2 (assoc m1 :k1 :v2)]
      (is (= 1 (count m1)))
      (is (= 1 (count m2)))
      (is (= :v1 (get m1 :k1)))
      (is (= :v2 (get m2 :k1)))))

  (testing "rebalances a red branch left assoc"
    (let [m1 (sorted-map 1 :v1 3 :v3)
          m2 (assoc m1 2 :v2)]
      (is (= 3 (count m2)))
      (is (contains? m2 1))
      (is (contains? m2 2))
      (is (contains? m2 3))))

  (testing "rebalances a red branch right on assoc"
    (let [m1 (sorted-map-by > 1 :v1 3 :v3)
          m2 (assoc m1 2 :v2)]
      (is (= 3 (count m2)))
      (is (contains? m2 1))
      (is (contains? m2 2))
      (is (contains? m2 3))))

  (testing "rebalances a black branch left on dissoc"
    (let [m1 (sorted-map 3 :v3 2 :v2 4 :v4 1 :v1)
          m2 (dissoc m1 2)]
      (is (= 3 (count m2)))
      (is (contains? m2 1))
      (is (contains? m2 3))
      (is (contains? m2 4))))

  (testing "rebalances a black branch right on dissoc"
    (let [m1 (sorted-map 2 :v2 1 :v1 3 :v3 4 :v4)
          m2 (dissoc m1 3)]
      (is (= 3 (count m2)))
      (is (contains? m2 1))
      (is (contains? m2 2))
      (is (contains? m2 4)))))

(deftest sorted-map-generative-test
  (defn- random-partition [coll remove-size]
    (loop [c coll
          removed-c '()
          counter remove-size]
      (if (zero? counter)
        [c removed-c]
        (let [rand-elem (rand-nth c)]
          (recur
            (remove #(= rand-elem %) c)
            (conj removed-c rand-elem)
            (dec counter))))))

  (testing "random assoc and dissoc combinations"
    (let [keyvals (repeatedly 100 #(rand-int 999))
          m1 (reduce #(assoc %1 %2 %2) (sorted-map) keyvals)
          [ks removed-ks] (random-partition keyvals 25)
          uniq-ks (distinct ks)
          m2 (reduce #(dissoc %1 %2) m1 removed-ks)]
      (is (= (clojure.core/count uniq-ks) (count m2)))
      (loop [expected-ks (sort uniq-ks)
             mseq (seq m2)]
        (if expected-ks
          (do
            (is (= (clojure.core/first expected-ks) (key (first mseq))))
            (recur (clojure.core/next expected-ks) (next mseq))))))))

(deftest sorted-map-seq-test
  (testing "seq return nil when the map is empty"
    (is (nil? (seq (sorted-map)))))

  (testing "first give a map entry with the key and val"
    (let [m1 (sorted-map :k1 1)
          m1-seq (seq m1)
          entry (first m1-seq)]
      (is (= :k1 (key entry)))
      (is (= 1 (val entry)))))

  (testing "next returns nil when there is only one entry"
    (is (nil? (next (seq (sorted-map :k1 1))))))

  (testing "rest returns an empty list when there is only one entry"
    (is (= EMPTY-LIST (rest (seq (sorted-map :k1 1))))))

  (testing "returns items in default order with the default comparator"
    (let [m1 (sorted-map 2 :v2 3 :v3 1 :v1)
          m1-seq (seq m1)
          entry1 (first m1-seq)
          entry2 (first (next m1-seq))
          entry3 (first (next (next m1-seq)))]
      (is (= 1 (key entry1)))
      (is (= :v1 (val entry1)))
      (is (= 2 (key entry2)))
      (is (= :v2 (val entry2)))
      (is (= 3 (key entry3)))
      (is (= :v3 (val entry3)))))

  (testing "returns items in order when using a user supplied comparable"
    (let [m1 (sorted-map-by > 2 :v2 3 :v3 1 :v1)
          m1-seq (seq m1)
          entry1 (first m1-seq)
          entry2 (first (next m1-seq))
          entry3 (first (next (next m1-seq)))]
      (is (= 3 (key entry1)))
      (is (= :v3 (val entry1)))
      (is (= 2 (key entry2)))
      (is (= :v2 (val entry2)))
      (is (= 1 (key entry3)))
      (is (= :v1 (val entry3)))))

  (testing "count the items in a seq"
    (let [m1-seq (seq (sorted-map 1 :v1 2 :v2 3 :v3 4 :v4 5 :v5))
          m2-seq (next m1-seq)
          m3-seq (next m2-seq)
          m4-seq (next m3-seq)
          m5-seq (next m4-seq)
          m6-seq (next m5-seq)]
      (is (= 5 (count m1-seq)))
      (is (= 4 (count m2-seq)))
      (is (= 3 (count m3-seq)))
      (is (= 2 (count m4-seq)))
      (is (= 1 (count m5-seq))))))
