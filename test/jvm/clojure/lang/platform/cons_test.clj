(ns clojure.lang.platform.cons-test
  (:refer-clojure :only [let])
  (:require [clojure.next :refer :all]
            [clojure.test :refer :all])
  (:import [java.util ArrayList]))

(deftest cons-collection-test
  (testing "add is not supported on cons"
    (is (thrown? UnsupportedOperationException (.add (cons 1 (vector 2)) 3))))

  (testing "add all is not supported on cons"
    (is (thrown? UnsupportedOperationException (.addAll (cons 1 (vector 2)) (ArrayList.)))))

  (testing "clear is not supported on cons"
    (is (thrown? UnsupportedOperationException (.clear (cons 1 (vector 2))))))

  (testing "contains returns false whena cons does not contain an item"
    (is (false? (.contains (cons :bar (vector :baz)) :foo))))

  (testing "contains returns true when a cons does contain the item"
    (is (true? (.contains (cons :bar (vector :baz)) :bar)))
    (is (true? (.contains (cons :bar (vector :baz)) :baz))))

  (testing "containsAll returns false when the list does not contain all of the elements"
    (let [arr-list (ArrayList.)]
      (.add arr-list :bar)
      (.add arr-list :foo)
      (is (false? (.containsAll (cons :bar (vector :baz)) arr-list)))))

  (testing "containsAll returns true when the list does contain all of the elements"
    (let [arr-list1 (ArrayList.)
          arr-list2 (ArrayList.)]
      (.add arr-list1 :bar)
      (.add arr-list2 :bar)
      (.add arr-list2 :baz)
      (is (true? (.containsAll (cons :bar (vector :baz)) (ArrayList.))))
      (is (true? (.containsAll (cons :bar (vector :baz)) arr-list1)))
      (is (true? (.containsAll (cons :bar (vector :baz)) arr-list2)))))

  (testing "isEmpty returns false for cons"
    (is (false? (.isEmpty (cons :bar (vector :baz))))))

  (testing "remove is not supported on cons"
    (is (thrown? UnsupportedOperationException (.remove (cons :bar (vector :baz)) :foo))))

  (testing "removeAll is not supoorted on cons"
    (is (thrown? UnsupportedOperationException (.removeAll (cons :bar (vector :baz)) (ArrayList.)))))

  (testing "retainAll is not supported on cons"
    (is (thrown? UnsupportedOperationException (.retainAll (cons :bar (vector :baz)) (ArrayList.)))))

  (testing "size for cons"
    (is (= 1 (.size (cons :bar nil))))
    (is (= 2 (.size (cons :bar (vector :baz)))))
    (is (= 3 (.size (cons :bar (cons :baz (vector :foo)))))))

  (testing "toArray with no arguments returns an array containing all elements"
    (let [arr (.toArray (cons :bar (vector :baz)))]
      (is (= 2 (count arr)))
      (is (= :bar (first arr)))
      (is (= :baz (second arr)))
      (is (= "class [Ljava.lang.Object;" (str (type (.toArray (cons :bar (vector :baz)))))))))

  (testing "toArray will make a new array which preserves type when an array is too short"
    (let [argument-arr (into-array Long (vector 1))
          arr (.toArray (cons 1 (vector 2)) argument-arr)]
      (is (not (identical? argument-arr arr)))
      (is (= "class [Ljava.lang.Long;" (str (type arr))))
      (is (= 2 (count arr)))
      (is (= 1 (first arr)))
      (is (= 2 (second arr)))))

  (testing "toArray will make a clone which preserves type and marks the last element as nil when too long"
    (let [argument-arr (make-array Long 4)
          _ (aset argument-arr 3 42)
          arr (.toArray (cons 1 (vector 2)) argument-arr)]
      (is (not (identical? argument-arr arr)))
      (is (= "class [Ljava.lang.Long;" (str (type arr))))
      (is (= 4 (count arr)))
      (is (= 1 (aget arr 0)))
      (is (= 2 (aget arr 1)))
      (is (nil? (aget arr 2)))
      (is (= 42 (aget arr 3))))))

