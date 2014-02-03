(ns clojure.lang.persistent-hash-map-test
  (:refer-clojure :only [nil? let defn- loop])
  (:require [clojure.test                     :refer :all]
            [clojure.lang.map-entry           :refer [key val]]
            [clojure.lang.object              :refer [identical?]]
            [clojure.lang.persistent-map-test :refer [map-test]]
            [clojure.lang.persistent-hash-map :refer [hash-map]]
            [clojure.lang.persistent-hash-set :refer [hash-set]]
            [clojure.lang.persistent-set      :refer [union]]
            [clojure.next                     :refer :all]))

(deftest hash-map-test
  (map-test "PersistentHashMap" hash-map))

(deftest hash-map-seq-test
  (testing "seq returns nil when the map is empty"
    (is (nil? (seq (hash-map)))))

  (testing "first gives a map entry with the key and val"
    (let [m1 (hash-map :k1 1)
          m1-seq (seq m1)
          entry (first m1-seq)]
      (is (= :k1 (key entry)))
      (is (= 1 (val entry)))))

  (testing "next returns nil when there is only one entry"
    (is (nil? (next (seq (hash-map :k1 1))))))

  (testing "next returns the next entry when there more than one entry"
    (let [m1 (hash-map :k1 1 :k2 2)
          m1-seq (seq m1)
          m2-seq (next m1-seq)
          entry (first m2-seq)]
      (not (identical? m1-seq m2-seq))
      (is (= :k2 (key entry)))
      (is (= 2 (val entry)))))

  (testing "counts the items in a seq"
    (let [m1-seq (seq (hash-map :k1 1 :k2 2 :k3 3 :k4 4 :k5 5))
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
