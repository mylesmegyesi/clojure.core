(ns clojure.lang.platform.lazy-seq-test
  (:refer-clojure :only [let])
  (:require [clojure.test                 :refer :all]
            [clojure.lang.persistent-list :refer [list]]
            [clojure.next                 :refer :all])
  (:import [java.util ArrayList]))

(deftest lazy-seq-hash-test
  (testing "lazy seq hash"
    (is (= 30817 (.hashCode (lazy-seq (list 1 2 3)))))
    (is (= 1 (.hashCode (lazy-seq))))))

(deftest lazy-seq-collection-test
  (testing "add is not supported on empty lazy-seq"
    (is (thrown? UnsupportedOperationException (.add (lazy-seq) :foo))))

  (testing "add all is not supported on empty lazy-seq"
    (is (thrown? UnsupportedOperationException (.addAll (lazy-seq) (ArrayList.)))))

  (testing "clear is not supported on empty lazy-seq"
    (is (thrown? UnsupportedOperationException (.clear (lazy-seq)))))

  (testing "contains returns false for an empty lazy-seq"
    (is (false? (.contains (lazy-seq) :foo))))

  (testing "contains returns false when the lazy-seq does not contain the item"
    (is (false? (.contains (lazy-seq (cons :foo nil)) :bar))))

  (testing "contains returns true when the lazy-seq contains the item"
    (is (true? (.contains (lazy-seq (cons :foo nil)) :foo))))

  (testing "containsAll returns false when the lazy-seq does not contain all of the elements"
    (let [arr-list (ArrayList.)]
      (.add arr-list :bar)
      (.add arr-list :foo)
      (is (false? (.containsAll (lazy-seq (vector :bar :baz)) arr-list)))))

  (testing "containsAll returns true when the list does contain all of the elements"
    (let [arr-list1 (ArrayList.)
          arr-list2 (ArrayList.)]
      (.add arr-list1 :bar)
      (.add arr-list2 :bar)
      (.add arr-list2 :baz)
      (is (true? (.containsAll (lazy-seq (vector :bar :baz)) (ArrayList.))))
      (is (true? (.containsAll (lazy-seq (vector :bar :baz)) arr-list1)))
      (is (true? (.containsAll (lazy-seq (vector :bar :baz)) arr-list2)))))

  (testing "isEmpty returns false for a non-empty lazy-seq"
    (is (false? (.isEmpty (lazy-seq (cons :foo nil))))))

  (testing "isEmpty returns true for an empty lazy-seq"
    (is (true? (.isEmpty (lazy-seq)))))

  (testing "remove is not supported on lazy-seq"
    (is (thrown? UnsupportedOperationException (.remove (lazy-seq) :foo))))

  (testing "removeAll is not supported on lazy-seq"
    (is (thrown? UnsupportedOperationException (.removeAll (lazy-seq) (ArrayList.)))))

  (testing "retainAll is not supported on lazy-seq"
    (is (thrown? UnsupportedOperationException (.retainAll (lazy-seq) (ArrayList.)))))

  (testing "size for lazy-seq"
    (is (= 1 (.size (lazy-seq (cons :foo nil)))))
    (is (= 2 (.size (lazy-seq (vector :foo :bar)))))
    (is (= 3 (.size (lazy-seq (vector :foo :bar :baz))))))

  (testing "toArray with no arguments returns an array containing all elements"
    (let [arr (.toArray (lazy-seq (vector :bar :baz)))]
      (is (= 2 (count arr)))
      (is (= :bar (first arr)))
      (is (= :baz (second arr)))
      (is (= "class [Ljava.lang.Object;" (str (type (.toArray (list :bar :baz))))))))

  (testing "toArray will make a new array which preserves type when an array is too short"
    (let [argument-arr (into-array Long (list 1))
          arr (.toArray (lazy-seq (vector 1 2)) argument-arr)]
      (is (not (identical? argument-arr arr)))
      (is (= "class [Ljava.lang.Long;" (str (type arr))))
      (is (= 2 (count arr)))
      (is (= 1 (first arr)))
      (is (= 2 (second arr)))))

  (testing "toArray will make a clone which preserves type and marks the last element as nil when too long"
    (let [argument-arr (make-array Long 4)
          _ (aset argument-arr 3 42)
          arr (.toArray (lazy-seq (vector 1 2)) argument-arr)]
      (is (not (identical? argument-arr arr)))
      (is (= "class [Ljava.lang.Long;" (str (type arr))))
      (is (= 4 (count arr)))
      (is (= 1 (aget arr 0)))
      (is (= 2 (aget arr 1)))
      (is (nil? (aget arr 2)))
      (is (= 42 (aget arr 3))))))

