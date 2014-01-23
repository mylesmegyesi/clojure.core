(ns clojure.lang.persistent-sorted-map-test
  (:refer-clojure :only [let nil? >])
  (:require [clojure.test                       :refer :all]
            [clojure.lang.counted               :refer [count]]
            [clojure.lang.map-entry             :refer [key val]]
            [clojure.lang.operators             :refer [=]]
            [clojure.lang.persistent-map-test   :refer [map-test]]
            [clojure.lang.persistent-sorted-map :refer :all]
            [clojure.lang.seq                   :refer [seq first next]]))

(deftest sorted-map-test
  (map-test "PersistentSortedMap" sorted-map))

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
