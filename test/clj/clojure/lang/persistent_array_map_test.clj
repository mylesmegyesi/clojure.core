(ns clojure.lang.persistent-array-map-test
  (:refer-clojure :only [deftype defmacro let nil? list list*])
  (:require [clojure.test                      :refer :all]
            [clojure.lang.comparison           :refer [= not=]]
            [clojure.lang.counted              :refer [count]]
            [clojure.lang.hash                 :refer [hash]]
            [clojure.lang.ihash                :refer [IHash]]
            [clojure.lang.logical              :refer [not]]
            [clojure.lang.map-entry            :refer [key val]]
            [clojure.lang.persistent-map       :refer [assoc dissoc get seq contains?]]
            [clojure.lang.persistent-array-map :refer :all]
            [clojure.lang.platform.object      :refer [identical?]]
            [clojure.lang.platform.exceptions  :refer [argument-error]]
            [clojure.lang.seq                  :refer [first next]]))

(defmacro argument-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? argument-error msg body)))

(deftest map-test

  (testing "creates a map with 0 items"
    (is (= 0 (count (array-map)))))

  (testing "creates a map with initial values"
    (let [m1 (array-map :k1 1)
          m2 (array-map :k1 1 :k2 2)]
      (is (= 1 (get m1 :k1)))
      (is (= nil (get m1 :k2)))
      (is (= 1 (count m1)))
      (is (= 1 (get m2 :k1)))
      (is (= 2 (get m2 :k2)))
      (is (= 2 (count m2)))))

  (testing "throws an exception if there are not an even number of arguements"
    (argument-error-is-thrown?
      #"PersistenArrayMap can only be created with even number of arguements: 3 arguements given"
      (array-map :k1 1 :k2)))

  (testing "associates a key to a value"
    (let [m1 (array-map)
          m2 (assoc m1 :key 1)]
      (is (not (identical? m1 m2)))
      (is (not= m1 m2))
      (is (nil? (get m1 :key)))
      (is (= 1 (get m2 :key)))
      (is (= 0 (count m1)))
      (is (= 1 (count m2)))))

  (testing "associates a key to a value when the key already exists"
    (let [m1 (array-map)
          m2 (assoc m1 :key 1)
          m3 (assoc m2 :key 2)]
      (is (not (identical? m2 m3)))
      (is (not= m2 m3))
      (is (nil? (get m1 :key)))
      (is (= 1 (get m2 :key)))
      (is (= 2 (get m3 :key)))
      (is (= 0 (count m1)))
      (is (= 1 (count m2)))
      (is (= 1 (count m3)))))

  (testing "associates a key to a value when the key already exists and the value is the same"
    (let [m1 (array-map)
          m2 (assoc m1 :key 1)
          m3 (assoc m2 :key 1)]
      (is (identical? m2 m3))))

  (testing "dissociates a key from a map with one item"
    (let [m1 (array-map :k1 1)
          m2 (dissoc m1 :k1)]
      (is (= 1 (count m1)))
      (is (= 1 (get m1 :k1)))
      (is (= 0 (count m2)))
      (is (= nil (get m2 :1)))))

  (testing "dissociates a key from a map with two items"
    (let [m1 (array-map :k1 1 :k2 2)
          m2 (dissoc m1 :k1)
          m3 (dissoc m1 :k2)]
      (is (not (identical? m1 m2)))
      (is (not (identical? m1 m3)))

      (is (= 2 (count m1)))
      (is (= 1 (get m1 :k1)))
      (is (= 2 (get m1 :k2)))

      (is (= 1 (count m2)))
      (is (nil? (get m2 :k1)))
      (is (= 2 (get m2 :k2)))

      (is (= 1 (count m3)))
      (is (= 1 (get m3 :k1)))
      (is (nil? (get m3 :k2)))))

  (testing "dissociates a key from a map with three items"
    (let [m1 (array-map :k1 1 :k2 2 :k3 3)
          m2 (dissoc m1 :k1)
          m3 (dissoc m1 :k2)
          m4 (dissoc m1 :k3)]
      (is (not (identical? m1 m2)))
      (is (not (identical? m1 m3)))
      (is (not (identical? m1 m4)))

      (is (= 3 (count m1)))
      (is (= 1 (get m1 :k1)))
      (is (= 2 (get m1 :k2)))
      (is (= 3 (get m1 :k3)))

      (is (= 2 (count m2)))
      (is (nil? (get m2 :k1)))
      (is (= 2 (get m2 :k2)))
      (is (= 3 (get m2 :k3)))

      (is (= 2 (count m3)))
      (is (= 1 (get m3 :k1)))
      (is (nil? (get m3 :k2)))
      (is (= 3 (get m3 :k3)))

      (is (= 2 (count m4)))
      (is (= 1 (get m4 :k1)))
      (is (= 2 (get m4 :k2)))
      (is (nil? (get m4 :k3)))))

  (testing "returns the map when there are not items to dissoc"
    (let [m1 (array-map :k1 1 :k2 2 :k3 3)]
      (is (identical? m1 (dissoc m1 :k4)))))

  (testing "contains? a key if the key is present in the map"
    (let [m1 (array-map :k1 1)]
      (is (contains? m1 :k1))))

  (testing "does not contains? a key if the key is not present in the map"
    (let [m1 (array-map :k1 1)]
      (is (not (contains? m1 :k2)))))

  (testing "does not contains? a key if the key is present AS A VALUE in the map"
    (let [m1 (array-map :k1 1)]
      (is (not (contains? m1 1)))))

  (testing "returns a provided not-found value when using get"
    (let [m1 (array-map)]
      (is (= "not found" (get m1 :not-a-key "not found")))))

  )

(deftest map-seq-test

  (testing "seq returns nil when the map is empty"
    (is (nil? (seq (array-map)))))

  (testing "first gives a map entry with the key and val"
    (let [m1 (array-map :k1 1)
          m1-seq (seq m1)
          entry (first m1-seq)]
      (is (= :k1 (key entry)))
      (is (= 1 (val entry)))))

  (testing "next returns nil when there is only one entry"
    (is (nil? (next (seq (array-map :k1 1))))))

  (testing "returns the first map entry when there are two items"
    (let [m1 (array-map :k1 1 :k2 2)
          m1-seq (seq m1)
          entry (first m1-seq)]
      (is (= :k1 (key entry)))
      (is (= 1 (val entry)))))

  (testing "next returns nil when there more than one entry"
    (let [m1 (array-map :k1 1 :k2 2)
          m1-seq (seq m1)
          m2-seq (next m1-seq)
          entry (first m2-seq)]
      (not (identical? m1-seq m2-seq))
      (is (= :k2 (key entry)))
      (is (= 2 (val entry)))))

  (testing "can keep calling next and eventually get to every item"
    (let [m1-seq (seq (array-map :k1 1 :k2 2 :k3 3 :k4 4 :k5 5))
          m2-seq (next m1-seq)
          m3-seq (next m2-seq)
          m4-seq (next m3-seq)
          m5-seq (next m4-seq)
          m6-seq (next m5-seq)]
      (is (= :k1 (key (first m1-seq))))
      (is (= 1   (val (first m1-seq))))
      (is (= :k2 (key (first m2-seq))))
      (is (= 2   (val (first m2-seq))))
      (is (= :k3 (key (first m3-seq))))
      (is (= 3   (val (first m3-seq))))
      (is (= :k4 (key (first m4-seq))))
      (is (= 4   (val (first m4-seq))))
      (is (= :k5 (key (first m5-seq))))
      (is (= 5   (val (first m5-seq))))
      (is (nil? m6-seq))))

  (testing "counts the items in a seq"
    (let [m1-seq (seq (array-map :k1 1 :k2 2 :k3 3 :k4 4 :k5 5))
          m2-seq (next m1-seq)
          m3-seq (next m2-seq)
          m4-seq (next m3-seq)
          m5-seq (next m4-seq)
          m6-seq (next m5-seq)]
      (is (= 5 (count m1-seq)))
      (is (= 4 (count m2-seq)))
      (is (= 3 (count m3-seq)))
      (is (= 2 (count m4-seq)))
      (is (= 1 (count m5-seq)))))

  )

(deftest map-equivalence-test

  (testing "equal if same keys and values"
    (let [m1 (array-map :k1 1 :k2 2)
          m2 (array-map :k1 1 :k2 2)]
      (is (= m1 m2))))

  (testing "not equal with same keys and different values"
    (let [m1 (array-map :k 1)
          m2 (array-map :k 2)]
      (is (not= m1 m2))))

  (testing "all keys in lhs have to be contained in rhs"
    (let [m1 (array-map :k1 nil)
          m2 (array-map :k2 1)]
      (is (not= m1 m2))))

  )

(deftype Thing [t]
  IHash
  (-hash [this] t))

(deftest map-hash-test
  (testing "the hash of an empty map-hash is zero"
    (let [m1 (array-map)]
      (is (= 0 (hash m1)))))

  (testing "calculates the bit-and of the key and value and adds them together"
    (let [thing1 (Thing. 2r1101)
          thing2 (Thing. 2r1010)
          m1 (array-map thing1 thing2)]
      (is (= 2r1000 (hash m1)))))

  (testing "calculates the bit-and of to map entries"
    (let [thing1 (Thing. 2r1101)
          thing2 (Thing. 2r1010)
          thing3 (Thing. 2r1011)
          thing4 (Thing. 2r1111)
          m1 (array-map thing1 thing2 thing3 thing4)]
      (is (= 2r10011 (hash m1)))))

  (testing "hash code of equivalent maps will be the same"
    (let [m1 (array-map :k 1)
          m2 (array-map :k 1)]
      (is (= (hash m1) (hash m2)))))

  (testing "hash code of non-equivalent maps will not be the same"
    (let [m1 (array-map :k1 1)
          m2 (array-map :k2 2)]
      (is (not (= (hash m1) (hash m2))))))

  )
