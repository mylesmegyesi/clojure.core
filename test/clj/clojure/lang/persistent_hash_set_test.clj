(ns clojure.lang.persistent-hash-set-test
  (:refer-clojure :only [deftype first let next nil?])
  (:require [clojure.test                     :refer :all]
            [clojure.lang.ihash               :refer [IHash]]
            [clojure.lang.counted             :refer [count]]
            [clojure.lang.hash                :refer [hash]]
            [clojure.lang.lookup              :refer [contains? get]]
            [clojure.lang.operators           :refer [not =]]
            [clojure.lang.persistent-set      :refer [conj difference disj intersection subset? superset? union]]
            [clojure.lang.persistent-hash-set :refer :all]
            [clojure.lang.seq                 :refer [seq]]))

(deftest persistent-hash-set-test
  (testing "an empty hash set does not contains? an item"
    (let [s1 (hash-set)]
      (is (not (contains? s1 :anything)))))

  (testing "a hash set contains? an item"
    (let [s1 (hash-set "item")]
      (is (contains? s1 "item"))))

  (testing "count set entries"
    (is (= 0 (count (hash-set))))
    (is (= 1 (count (hash-set :one))))
    (is (= 2 (count (hash-set :one :two)))))

  (testing "get returns the element if it is a member of the set and nil otherwise"
    (let [s1 (hash-set 1)]
      (is (= 1 (get s1 1)))
      (is (nil? (get s1 2)))))

  (testing "get returns a default value if not part of the set"
    (let [s1 (hash-set 1)]
      (is (= 1 (get s1 1 :default)))
      (is (= :default (get s1 2 :default)))))

  (testing "conj an item to a hash set"
    (let [s1 (hash-set)
          s2 (conj s1 "item")]
      (is (contains? s2 "item"))))

  (testing "conj many items to a hash set"
    (let [s1 (hash-set)
          s2 (conj s1 1 2 3)]
      (is (contains? s2 1))
      (is (contains? s2 2))
      (is (contains? s2 3))))

  (testing "disj from an empty set is identity"
    (let [s1 (hash-set)]
      (is (= s1 (disj s1)))))

  (testing "disj an entry from a set"
    (let [s1 (hash-set 1)]
      (is (= (hash-set) (disj s1 1)))))

  (testing "disj multiple entries from a set"
    (let [s1 (hash-set 1 2 3 4 5)
          s2 (hash-set 2 4)]
      (is (= s2 (disj s1 1 3 5)))))

  (testing "difference of a single set is identity"
    (let [s1 (hash-set 1 2 3)]
      (is (= s1 (difference s1)))))

  (testing "difference of two sets"
    (let [s1 (hash-set 1 2)
          s2 (hash-set 2 3)
          s3 (hash-set 1)]
      (is (= s3 (difference s1 s2)))))

  (testing "difference of many sets"
    (let [s1 (hash-set 1 2 3 4)
          s2 (hash-set 1)
          s3 (hash-set 4 5)
          s4 (hash-set 2 3)]
      (is (= s4 (difference s1 s2 s3)))))

  (testing "intersection of two sets"
    (let [s1 (hash-set 1 2 3 5)
          s2 (hash-set 1 3 4)]
      (is (= (hash-set 1 3) (intersection s1 s2)))))

  (testing "intersection of many sets"
    (let [s1 (hash-set 1 2 3 5)
          s2 (hash-set 1 3 5 6)
          s3 (hash-set 3 4 5 6)]
      (is (= (hash-set 3 5) (intersection s1 s2 s3)))))

  (testing "union without args returns a new set"
    (is (= (hash-set) (union))))

  (testing "union with a single set is identity"
    (let [s1 (hash-set 1 2 3)]
      (is (= s1 (union s1)))))

  (testing "union of two sets"
    (let [s1 (hash-set 1 2)
          s2 (hash-set 2 3)
          s3 (hash-set 1 2 3)]
      (is (= s3 (union s1 s2)))))

  (testing "union of many sets"
    (let [s1 (hash-set 1 2)
          s2 (hash-set :foo :bar)
          s3 (hash-set "baz" 3.14 1)
          s4 (hash-set 1 2 :foo :bar "baz" 3.14)]
      (is (= s4 (union s1 s2 s3)))))

  (testing "a set is a subset? of an equivalent set"
    (is (subset? (hash-set) (hash-set)))
    (is (subset? (hash-set :item) (hash-set :item))))

  (testing "a set is not a subset? of another"
    (let [s1 (hash-set 1 2 3)
          s2 (hash-set 2)]
      (is (not (subset? s1 s2)))))

  (testing "a set is a subset? of another"
    (let [s1 (hash-set 2)
          s2 (hash-set 1 2 3)]
      (is (subset? s1 s2))))

  (testing "a set is a superset? of an equivalent set"
    (is (superset? (hash-set) (hash-set)))
    (is (superset? (hash-set :item) (hash-set :item))))

  (testing "a set is not a superset? of another"
    (let [s1 (hash-set 2)
          s2 (hash-set 1 2 3)]
      (is (not (superset? s1 s2)))))

  (testing "a set is a subset? of another"
    (let [s1 (hash-set 1 2 32)
          s2 (hash-set 2)]
      (is (superset? s1 s2))))

  )

(deftype Thing [t]
  IHash
  (-hash [this] t))

(deftest persistent-hash-set-hash-test
  (testing "the hash of an empty set is zero"
    (let [s1 (hash-set)]
      (is (= 0 (hash s1)))))

  (testing "calculates the sum of all of it's elements hash codes"
    (let [thing1 (Thing. 42)
          thing2 (Thing. 24)
          thing3 (Thing. 1337)
          s1 (hash-set thing1 thing2 thing3)]
      (is (= 1403 (hash s1)))))

  (testing "hash of two identical sets will be the same"
    (let [s1 (hash-set 1 2 3)
          s2 (hash-set 1 2 3)]
      (is (= (hash s1) (hash s2)))))
  )

(deftest persistent-hash-set-equality-test
  (testing "two sets are equal"
    (is (= (hash-set) (hash-set)))
    (is (= (hash-set 1 2) (hash-set 1 2))))

  (testing "a superset is not equal to its subset"
    (is (not (= (hash-set 1 2) (hash-set 1))))
    (is (not (= (hash-set 1)   (hash-set 1 2)))))

  )

(deftest persistent-hash-set-seq-test
  (testing "seq returns nil when the set is empty"
    (is (nil? (seq (hash-set)))))

  (testing "first gives a set element"
    (let [s1 (hash-set 1)]
      (is (= 1 (first (seq s1))))))

  (testing "next returns nil when there is only one entry"
    (is (nil? (next (seq (hash-set 1))))))

  (testing "next returns the next sequence"
    (is (= 2 (first (next (seq (hash-set 1 2)))))))

  )
