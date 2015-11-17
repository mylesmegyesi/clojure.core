(ns clojure.lang.platform.persistent-vector-test
  (:refer-clojure :only [let])
  (:require [clojure.test                 :refer :all]
            [clojure.lang.persistent-list :refer [list]]
            [clojure.next                 :refer :all])
  (:import [java.util ArrayList]))

(deftest sub-vec-collection-test
  (testing "add is not supported on subvecs"
    (is (thrown? UnsupportedOperationException (.add (subvec (vector :foo :bar) 1) :baz))))

  (testing "add all is not supported on subvecs"
    (is (thrown? UnsupportedOperationException (.addAll (subvec (vector :foo :bar) 1) (ArrayList.)))))

  (testing "clear is not supported on subvecs"
    (is (thrown? UnsupportedOperationException (.clear (subvec (vector :foo :bar) 1)))))

  (testing "contains returns false when a subvec does not contain the item"
    (is (false? (.contains (subvec (vector :foo :bar) 1) :baz))))

  (testing "contains returns true when a subvec does contain the item"
    (is (true? (.contains (subvec (vector :foo :bar) 1) :bar))))

  (testing "containsAll returns false when a subvec does not contain all of the elements"
    (let [arr-list (ArrayList.)]
      (.add arr-list :bar)
      (.add arr-list :foo)
      (is (false? (.containsAll (subvec (vector :foo :bar) 1) arr-list)))))

  (testing "containsAll returns true when a subvec does contain all of the elements"
    (let [arr-list1 (ArrayList.)
          arr-list2 (ArrayList.)]
      (.add arr-list1 :bar)
      (.add arr-list2 :bar)
      (.add arr-list2 :baz)
      (is (true? (.containsAll (subvec (vector :foo :bar :baz) 1) (ArrayList.))))
      (is (true? (.containsAll (subvec (vector :foo :bar :baz) 1) arr-list1)))
      (is (true? (.containsAll (subvec (vector :foo :bar :baz) 1) arr-list2)))))

  ; can't create an empty subvec since it's just the empty vector
  (testing "isEmpty returns true for a non empty subvec"
    (is (false? (.isEmpty (subvec (vector :foo :bar) 1)))))

  (testing "remove is not supported on subvecs"
    (is (thrown? UnsupportedOperationException (.remove (subvec (vector :foo :bar) 1) :foo))))

  (testing "removeAll is not supported on subvecs"
    (is (thrown? UnsupportedOperationException (.removeAll (subvec (vector :foo :bar) 1) (ArrayList.)))))

  (testing "retainAll is not supported on subvecs"
    (is (thrown? UnsupportedOperationException (.retainAll (subvec (vector :foo :bar) 1) (ArrayList.)))))

  (testing "size for a subvec"
    (is (= 1 (.size (subvec (vector :foo :bar) 1))))
    (is (= 2 (.size (subvec (vector :foo :bar :baz) 1))))
    (is (= 3 (.size (subvec (vector :foo :bar :baz :bang) 1)))))

  (testing "toArray with no arguments returns an array containing all elements"
    (let [arr (.toArray (subvec (vector :foo :bar :baz) 1))]
      (is (= 2 (count arr)))
      (is (= :bar (first arr)))
      (is (= :baz (second arr)))
      (is (= "class [Ljava.lang.Object;" (str (type (.toArray (subvec (vector :foo :bar :baz) 1))))))))

  (testing "toArray will make a new array which preserves type when an array is too short"
    (let [argument-arr (into-array Long (vector 1))
          arr (.toArray (subvec (vector 0 1 2) 1) argument-arr)]
      (is (not (identical? argument-arr arr)))
      (is (= "class [Ljava.lang.Long;" (str (type arr))))
      (is (= 2 (count arr)))
      (is (= 1 (first arr)))
      (is (= 2 (second arr)))))

  (testing "toArray wil make a clone which preserves type and marks the last element as nil when too long"
    (let [argument-arr (make-array Long 4)
          _ (aset argument-arr 3 42)
          arr (.toArray (subvec (vector 0 1 2) 1) argument-arr)]
      (is (not (identical? argument-arr arr)))
      (is (= "class [Ljava.lang.Long;" (str (type arr))))
      (is (= 4 (count arr)))
      (is (= 1 (aget arr 0)))
      (is (= 2 (aget arr 1)))
      (is (nil? (aget arr 2)))
      (is (= 42 (aget arr 3))))))

(deftest vector-hash-test
  (testing "vector hash"
    (is (= 30817 (.hashCode (subvec (vector nil 1 2 3) 1))))
    (is (= -366456230 (.hashCode (subvec (vector nil :foo :bar :baz) 1))))
    (is (= 32 (.hashCode (subvec (vector nil (vector)) 1))))))

(deftest vector-collection-test
  (testing "add is not supported on vectors"
    (is (thrown? UnsupportedOperationException (.add (vector :bar) :baz))))

  (testing "add all is not supported on vectors"
    (is (thrown? UnsupportedOperationException (.addAll (vector :bar) (ArrayList.)))))

  (testing "clear is not supported on vectors"
    (is (thrown? UnsupportedOperationException (.clear (vector :bar)))))

  (testing "contains returns false when a vector does not contain the item"
    (is (false? (.contains (vector :bar) :baz))))

  (testing "contains returns true when a vector does contain the item"
    (is (true? (.contains (vector :bar) :bar))))

  (testing "containsAll returns false when a vector does not contain all of the elements"
    (let [arr-list (ArrayList.)]
      (.add arr-list :bar)
      (.add arr-list :foo)
      (is (false? (.containsAll (vector :bar) arr-list)))))

  (testing "containsAll returns true when a vector does contain all of the elements"
    (let [arr-list1 (ArrayList.)
          arr-list2 (ArrayList.)]
      (.add arr-list1 :bar)
      (.add arr-list2 :bar)
      (.add arr-list2 :baz)
      (is (true? (.containsAll (vector :bar :baz) (ArrayList.))))
      (is (true? (.containsAll (vector :bar :baz) arr-list1)))
      (is (true? (.containsAll (vector :bar :baz) arr-list2)))))

  ; can't create an empty vector since it's just the empty vector
  (testing "isEmpty returns true for a non empty vector"
    (is (false? (.isEmpty (vector :bar)))))

  (testing "remove is not supported on vectors"
    (is (thrown? UnsupportedOperationException (.remove (vector :bar) :foo))))

  (testing "removeAll is not supported on vectors"
    (is (thrown? UnsupportedOperationException (.removeAll (vector :bar) (ArrayList.)))))

  (testing "retainAll is not supported on vectors"
    (is (thrown? UnsupportedOperationException (.retainAll (vector :bar) (ArrayList.)))))

  (testing "size for a vector"
    (is (= 1 (.size (vector :bar))))
    (is (= 2 (.size (vector :bar :baz))))
    (is (= 3 (.size (vector :bar :baz :bang)))))

  (testing "toArray with no arguments returns an array containing all elements"
    (let [arr (.toArray (vector :bar :baz))]
      (is (= 2 (count arr)))
      (is (= :bar (first arr)))
      (is (= :baz (second arr)))
      (is (= "class [Ljava.lang.Object;" (str (type (.toArray (vector (vector :foo :bar :baz) 1))))))))

  (testing "toArray will make a new array which preserves type when an array is too short"
    (let [argument-arr (into-array Long (vector 1))
          arr (.toArray (vector 1 2) argument-arr)]
      (is (not (identical? argument-arr arr)))
      (is (= "class [Ljava.lang.Long;" (str (type arr))))
      (is (= 2 (count arr)))
      (is (= 1 (first arr)))
      (is (= 2 (second arr)))))

  (testing "toArray wil make a clone which preserves type and marks the last element as nil when too long"
    (let [argument-arr (make-array Long 4)
          _ (aset argument-arr 3 42)
          arr (.toArray (vector  1 2) argument-arr)]
      (is (not (identical? argument-arr arr)))
      (is (= "class [Ljava.lang.Long;" (str (type arr))))
      (is (= 4 (count arr)))
      (is (= 1 (aget arr 0)))
      (is (= 2 (aget arr 1)))
      (is (nil? (aget arr 2)))
      (is (= 42 (aget arr 3))))))

(deftest vector-hash-test
  (testing "an empty vector has a hash code of 1"
    (is (= 1 (.hashCode (vector)))))

  (testing "vector hash"
    (is (= 30817 (.hashCode (vector 1 2 3))))
    (is (= -366456230 (.hashCode (vector :foo :bar :baz))))
    (is (= 32 (.hashCode (vector (vector)))))))

