(ns clojure.lang.persistent-map-test
  (:refer-clojure :only [defmacro deftype defn let list list* nil? re-pattern])
  (:require [clojure.test                       :refer :all]
            [clojure.lang.counted               :refer [count]]
            [clojure.lang.icomparable           :refer [IComparable]]
            [clojure.lang.ihash                 :refer [IHash]]
            [clojure.lang.hash                  :refer [hash]]
            [clojure.lang.lookup                :refer [contains? get]]
            [clojure.lang.meta                  :refer [meta with-meta]]
            [clojure.lang.operators             :refer [not not= =]]
            [clojure.lang.persistent-map        :refer [assoc dissoc]]
            [clojure.lang.persistent-sorted-map :refer :all]
            [clojure.lang.platform.comparison]
            [clojure.lang.platform.exceptions   :refer [argument-error]]
            [clojure.lang.platform.object       :refer [identical?]]
            [clojure.lang.show                  :refer [str]]))

(defmacro argument-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? argument-error msg body)))

(defn map-creation-test [class-name constructor]
  (testing "creates a map with 0 items"
    (is (= 0 (count (constructor)))))

  (testing "creates a map with initial values"
    (let [m1 (constructor :k1 1)
          m2 (constructor :k1 1 :k2 2)]
      (is (= 1 (get m1 :k1)))
      (is (= nil (get m1 :k2)))
      (is (= 1 (count m1)))
      (is (= 1 (get m2 :k1)))
      (is (= 2 (get m2 :k2)))
      (is (= 2 (count m2)))))

  (testing
    "throws an exception if there are not an even number of arguements"
    (let [msg (re-pattern (str class-name " can only be created with even number of arguments: 3 arguments given"))]
      (argument-error-is-thrown? msg (constructor :k1 1 :k2)))))

(defn map-assoc-test [constructor]
  (testing "associates a key to a value"
    (let [m1 (constructor)
          m2 (assoc m1 :key 1)]
      (is (not (identical? m1 m2)))
      (is (not= m1 m2))
      (is (nil? (get m1 :key)))
      (is (= 1 (get m2 :key)))
      (is (= 0 (count m1)))
      (is (= 1 (count m2)))))

  (testing "associates a key to a value when the key already exists"
    (let [m1 (constructor)
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
    (let [m1 (constructor)
          m2 (assoc m1 :key 1)
          m3 (assoc m2 :key 1)]
      (is (identical? m2 m3))))

  (testing "associates many keys to their values"
    (let [m1 (constructor :k1 1)
          m2 (assoc m1 :k2 2 :k3 3 :k1 4)]
      (is (= 4 (get m2 :k1)))
      (is (= 2 (get m2 :k2)))
      (is (= 3 (get m2 :k3))))))

(defn map-dissoc-test [constructor]
  (testing "dissociating zero keys from a map will return the same map"
    (let [m1 (constructor)
          m2 (dissoc m1)]
      (is (identical? m1 m2))))

  (testing "dissociates a key from a map with one item"
    (let [m1 (constructor :k1 1)
          m2 (dissoc m1 :k1)]
      (is (= 1 (count m1)))
      (is (= 1 (get m1 :k1)))
      (is (= 0 (count m2)))
      (is (= nil (get m2 :1)))))

  (testing "dissociates a key from a map with two items"
    (let [m1 (constructor :k1 1 :k2 2)
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
    (let [m1 (constructor :k1 1 :k2 2 :k3 3)
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

  (testing "dissociates many keys"
    (let [m1 (constructor :k1 1 :k2 2 :k3 3)
          m2 (dissoc m1 :k1 :k3 :k4)]
      (is (= 2 (get m2 :k2)))
      (is (not (contains? m2 :k1)))
      (is (not (contains? m2 :k3)))))

  (testing "returns the map when there are not items to dissoc"
    (let [m1 (constructor :k1 1 :k2 2 :k3 3)]
      (is (identical? m1 (dissoc m1 :k4))))))

(defn map-contains?-test [constructor]
  (testing "contains? a key if the key is present in the map"
    (let [m1 (constructor :k1 1 :k2 2)]
      (is (contains? m1 :k1))
      (is (contains? m1 :k2))))

  (testing "does not contains? a key if the key is not present in the map"
    (let [m1 (constructor :k1 1)]
      (is (not (contains? m1 :k2)))))

  (testing "does not contains? a key if the key is present AS A VALUE in the map"
    (let [m1 (constructor :k1 :v1)]
      (is (not (contains? m1 :v1)))))

  (testing "returns a provided not-found value when using get"
    (let [m1 (constructor)]
      (is (= "not found" (get m1 :not-a-key "not found"))))))

(defn map-equivalence-test [constructor]
  (testing "equal if same keys and values"
    (let [m1 (constructor :k1 1 :k2 2)
          m2 (constructor :k1 1 :k2 2)]
      (is (= m1 m2))))

  (testing "not equal with same keys and different values"
    (let [m1 (constructor :k 1)
          m2 (constructor :k 2)]
      (is (not= m1 m2))))

  (testing "all keys in lhs have to be contained in rhs"
    (let [m1 (constructor :k1 nil)
          m2 (constructor :k2 1)]
      (is (not= m1 m2)))))

(defn map-meta-test [constructor]
  (testing "meta is nil after creation"
    (is (nil? (meta (constructor)))))

  (testing "with-meta returns a map with the specified meta"
    (let [mta {:so :meta}
          m1 (constructor)
          m2 (with-meta m1 mta)]
      (is (= mta (meta m2))))))

(deftype Thing [t]
  IHash
  (-hash [this] t)

  IComparable
  (-compare-to [this other] 1))

(defn map-hash-test [constructor]
  (testing "the hash of an empty map-hash is zero"
    (let [m1 (constructor)]
      (is (= 0 (hash m1)))))

  (testing "calculates the bit-and of the key and value and adds them together"
    (let [thing1 (Thing. 2r1101)
          thing2 (Thing. 2r1010)
          m1 (constructor thing1 thing2)]
      (is (= 2r1000 (hash m1)))))

  (testing "calculates the bit-and of to map entries"
    (let [thing1 (Thing. 2r1101)
          thing2 (Thing. 2r1010)
          thing3 (Thing. 2r1011)
          thing4 (Thing. 2r1111)
          m1 (constructor thing1 thing2 thing3 thing4)]
      (is (= 2r10011 (hash m1)))))

  (testing "hash code of equivalent maps will be the same"
    (let [m1 (constructor :k 1)
          m2 (constructor :k 1)]
      (is (= (hash m1) (hash m2)))))

  (testing "hash code of non-equivalent maps will not be the same"
    (let [m1 (constructor :k1 1)
          m2 (constructor :k2 2)]
      (is (not (= (hash m1) (hash m2)))))))

(defn map-test [class-name constructor]
  (map-creation-test class-name constructor)
  (map-assoc-test constructor)
  (map-dissoc-test constructor)
  (map-contains?-test constructor)
  (map-equivalence-test constructor)
  (map-meta-test constructor)
  (map-hash-test constructor))
