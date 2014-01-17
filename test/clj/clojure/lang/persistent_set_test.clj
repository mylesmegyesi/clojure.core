(ns clojure.lang.persistent-set-test
  (:refer-clojure :only [deftype let])
  (:require [clojure.test                       :refer :all]
            [clojure.lang.lookup                :refer [contains?]]
            [clojure.lang.persistent-set        :refer [set]]
            [clojure.lang.persistent-array-map  :refer [array-map]]
            [clojure.lang.map-entry             :refer [make-map-entry]]
            [clojure.lang.persistent-hash-set   :refer [hash-set hash-set?]]
            [clojure.lang.persistent-sorted-set :refer [sorted-set]]))

(deftest persistent-set-test
  (testing "set produces a new hash-set from a seq"
    (let [s1 (set (array-map :a 1 :b 2))]
      (is (hash-set? s1))
      (is (contains? s1 (make-map-entry :a 1)))
      (is (contains? s1 (make-map-entry :b 2)))))
  )
