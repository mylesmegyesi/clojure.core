(ns clojure.lang.persistent-list-test
  (:refer-clojure :only [])
  (:require [clojure.test                         :refer :all]
            [clojure.lang.persistent-list         :refer [list]]
            [clojure.support.exception-assertions :refer [unsupported-error-is-thrown?]]
            [clojure.support.test-seq             :refer [test-seq]]
            [clojure.next                         :refer :all])
  (:import [java.util ArrayList]))

(deftest empty-list-equals-test
  (testing "empty list is equal to another empty list"
    (is (true? (.equals (list) (list)))))

  (testing "empty list is equal to another empty sequential"
    (is (true? (.equals (list) (test-seq '())))))

  (testing "an empty list is not equal to a non-empty list"
    (is (false? (.equals (list) (list :foo))))))

(deftest empty-list-collection-test
  (testing "add is not supported on empty list"
    (unsupported-error-is-thrown? #"" (.add (list) :foo)))

  (testing "add all is not supported on empty list"
    (unsupported-error-is-thrown? #"" (.addAll (list) (ArrayList.))))

  (testing "clear is not supported on empty list"
    (unsupported-error-is-thrown? #"" (.clear (list))))

  (testing "contains returns false for empty list"
    (is (false? (.contains (list) :foo))))

  (testing "containsAll returns true when the argument is empty for an empty list"
    (is (true? (.containsAll (list) (ArrayList.)))))

  (testing "containsAll returns false when the argument is not empty for an empty list"
    (let [arrList (ArrayList.)]
      (.add arrList :foo)
      (is (false? (.containsAll (list) arrList)))))

  (testing "isEmpty returns true for an empty list"
    (is (true? (.isEmpty (list)))))

  (testing "remove is not supported on empty list"
    (unsupported-error-is-thrown? #"" (.remove (list) :foo)))

  (testing "removeAll is not supported on empty list"
    (unsupported-error-is-thrown? #"" (.removeAll (list) (ArrayList.))))

  (testing "retainAll is not supported on empty list"
    (unsupported-error-is-thrown? #"" (.retainAll (list) (ArrayList.))))

  (testing "size is zero for an empty list"
    (is (zero? (.size (list)))))

  (testing "toArray with no arguments returns an empty array for empty list"
    (let [arr (.toArray (list))]
      (is (zero? (count arr)))))

  (testing "toArray returns the same array argument for empty list"
    (let [arr (object-array 0)]
      (is (identical? arr (.toArray (list) arr)))))

  (testing "toArray sets the first element of an array argument to nil for empty list"
    (let [arr (object-array (vector 1 2 3))]
      (.toArray (list) arr)
      (is (nil? (nth arr 0)))
      (is (= 2 (nth arr 1)))
      (is (= 3 (nth arr 2))))))

(deftest list-equals-test
  (testing "lists with items are equal to each other"
    (is (true? (.equals (list 1 2 3) (list 1 2 3)))))

  (testing "lists without equal items are not equal"
    (is (false? (.equals (list 1 2 3) (list 1))))))

(deftest list-collection-test
  (testing "add is not supported on lists"
    (unsupported-error-is-thrown? #"" (.add (list :bar :baz) :foo)))

  (testing "add all is not supported on lists"
    (unsupported-error-is-thrown? #"" (.addAll (list :bar :baz) (ArrayList.))))

  (testing "clear is not supported on lists"
    (unsupported-error-is-thrown? #"" (.clear (list :bar :baz))))

  (testing "contains returns false when a list does not contain the item"
    (is (false? (.contains (list :bar :baz) :foo))))

  (testing "contains returns true whena list does contain the item"
    (is (true? (.contains (list :bar :baz) :bar)))
    (is (true? (.contains (list :bar :baz) :baz))))

  (testing "containsAll returns false when the list does not contain all of the elements"
    (let [arrList (ArrayList.)]
      (.add arrList :bar)
      (.add arrList :foo)
      (is (false? (.containsAll (list :bar :baz) arrList)))))

  (testing "containsAll returns true when the list does contain all of the elements"
    (let [arrList1 (ArrayList.)
          arrList2 (ArrayList.)]
      (.add arrList1 :bar)
      (.add arrList2 :bar)
      (.add arrList2 :baz)
      (is (true? (.containsAll (list :bar :baz) (ArrayList.))))
      (is (true? (.containsAll (list :bar :baz) arrList1)))
      (is (true? (.containsAll (list :bar :baz) arrList2)))))

  (testing "isEmpty returns false for a non-empty list"
    (is (false? (.isEmpty (list :bar :baz)))))

  (testing "remove is not supported on lists"
    (unsupported-error-is-thrown? #"" (.remove (list :bar :baz) :foo)))

  (testing "removeAll is not supported on lists"
    (unsupported-error-is-thrown? #"" (.removeAll (list :bar :baz) (ArrayList.))))

  (testing "retainAll is not supported on lists"
    (unsupported-error-is-thrown? #"" (.retainAll (list :bar :baz) (ArrayList.))))

  (testing "size for lists"
    (is (= 1 (.size (list :bar))))
    (is (= 2 (.size (list :bar :baz))))
    (is (= 3 (.size (list :bar :baz :foo)))))

  (testing "toArray with no arguments returns an array containing all elements"
    (let [arr (.toArray (list :bar :baz))]
      (is (= 2 (count arr)))
      (is (= :bar (first arr)))
      (is (= :baz (second arr)))
      (is (= "class [Ljava.lang.Object;" (type (.toArray (list :bar :baz)))))))

  (testing "toArray will make a new array which preserves type when an array is too short"
    (let [argument-arr (int-array 1)
          arr (.toArray (list 1 2) argument-arr)]
      (is (not (identical? argument-arr arr)))
      (is (= "class [I" (str (type arr))))
      (is (= 2 (count arr)))
      (is (= 1 (first arr)))
      (is (= 2 (second arr)))))

  (testing "toArray will make a clone which preserves type and marks the last element as nil when too long"
    (let [argument-arr (int-array 4)
          (aset argument-arr 3 42)
          arr (.toArray (list 1 2) argument-arr)]
      (is (not (identical? argument-arr arr)))
      (is (= "class [I" (str (type arr))))
      (is (= 5 (count arr)))
      (is (= 1 (first arr)))
      (is (= 2 (second arr)))
      (is (nil? (nth arr 2)))
      (is (= 42 (nth arr 3))))))

