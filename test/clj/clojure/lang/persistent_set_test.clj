(ns clojure.lang.persistent-set-test
  (:refer-clojure :only [deftype let seq type])
  (:require [clojure.test                       :refer :all]
            [clojure.lang.persistent-set        :refer :all]
            [clojure.lang.persistent-hash-set   :refer [hash-set]]
            [clojure.lang.persistent-sorted-set :refer [sorted-set]]
            [clojure.lang.protocols             :refer [ISeqable]]
            [clojure.next                       :refer [contains?]]))

(deftype FakeSeqColl [coll]
  ISeqable
  (-seq [this] (seq coll)))

(deftest persistent-set-test
  (testing "set produces a new hash-set from a seq"
    (let [s1 (set (FakeSeqColl. '(1 2 3)))]
      (is 'PersistentHashSet (type s1))
      (is (contains? s1 1))
      (is (contains? s1 2))
      (is (contains? s1 3))))

  )
