(ns clojure.lang.platform.array-test
  (:refer-clojure :only [let reify])
  (:require [clojure.test       :refer :all]
            [clojure.next       :refer :all]
            [clojure.lang.array :refer [object-array-type]])
  (:import [java.util HashMap]))

(deftest to-array-test
  (testing "an object array returns itself"
    (let [arr (object-array 3)]
      (is (identical? arr (to-array arr)))))

  (testing "collections return the result of calling to array"
    (let [coll (vector 0 1 2)
          arr (to-array coll)]
      (is (= 3 (count arr)))
      (is (= 0 (aget arr 0)))
      (is (= 1 (aget arr 1)))
      (is (= 2 (aget arr 2)))))

  (testing "iterables return all iterable elements into an array"
    (let [called-count (atom 0)
          iterator (reify java.util.Iterator
                     (hasNext [this]
                       (>= 2 (deref called-count)))
                     (next [this]
                       (swap! called-count inc))
                     (remove [this]))
          iterable (reify java.lang.Iterable
                     (iterator [this] iterator))
          arr (to-array iterable)]
      (is (= 3 (count arr)))
      (is (= 1 (aget arr 0)))
      (is (= 2 (aget arr 1)))
      (is (= 3 (aget arr 2)))))

  (testing "maps returns an array of their entry set"
    (let [m (HashMap.)]
      (.put m 0 1)
      (.put m 2 3)
      (let [arr (to-array m)]
        (is (= 2 (count arr)))
        (is (= 0 (key (first arr))))
        (is (= 2 (key (second arr))))
        (is (= 1 (val (first arr))))
        (is (= 3 (val (second arr)))))))

  (testing "strings return an array of their characters"
    (let [arr (to-array "ary")]
      (is (= 3 (count arr)))
      (is (= \a (aget arr 0)))
      (is (= \r (aget arr 1)))
      (is (= \y (aget arr 2)))))

  (testing "non-object arrays are copied into an object array"
    (let [long-arr (long-array (vector 0 1 2))
          arr (to-array long-arr)]
      (is (= 3 (count arr)))
      (is (= 0 (aget arr 0)))
      (is (= 1 (aget arr 1)))
      (is (= 2 (aget arr 2)))
      (is (= object-array-type (type arr)))))

  (testing "a runtime exception is thrown for other types"
    (is (thrown? RuntimeException #"Unable to convert: java.lang.Long to Object\[\]" (to-array 1)))))

