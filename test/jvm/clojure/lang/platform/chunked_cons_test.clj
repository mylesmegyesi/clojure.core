(ns clojure.lang.platform.chunked-cons-test
  (:refer-clojure :only [defn- let loop when])
  (:require [clojure.test                 :refer :all]
            [clojure.lang.persistent-list :refer [list]]
            [clojure.next                 :refer :all])
  (:import [java.util ArrayList]))

(defn- make-chunk [seqable]
  (let [cb (chunk-buffer (count seqable))]
    (loop [s (seq seqable)]
      (when s
        (chunk-append cb s)
        (recur (next s))))
    (chunk cb)))

(deftest chunked-cons-equals-test
  (testing "chunked-cons with equal items are equal to each other"
    (is (true? (.equals (chunk-cons (make-chunk (list 1 2 3)) (list 4 5))
                        (chunk-cons (make-chunk (list 1 2 3)) (list 4 5))))))

  (testing "chunked-cons without equal items are not equal to each other"
    (is (false? (.equals (chunk-cons (make-chunk (list 1)) (list 2))
                         (chunk-cons (make-chunk (list 2)) (list 3)))))))

;TODO: Uncomment when chunked-seq has .hashCode
;(deftest chunked-cons-hash-code-test
;  (testing "chunked-cons hash"
;    (is (= 62 (.hashCode (chunk-cons (make-chunk (vector 0)) nil))))
;    (is (= -1749986983 (.hashCode (chunk-cons (make-chunk (vector :foo :bar)) (list :baz)))))))

(deftest chunked-cons-collection-test
  (testing "add is not supported on chunked-cons"
    (is (thrown? UnsupportedOperationException (.add (chunk-cons (make-chunk (list 1)) (list 2)) 3))))

  (testing "add all is not supported on chunked-cons"
    (is (thrown? UnsupportedOperationException (.addAll (chunk-cons (make-chunk (list 1)) (list 2)) (ArrayList.)))))

  (testing "clear is not supported on chunked-cons"
    (is (thrown? UnsupportedOperationException (.clear (chunk-cons (make-chunk (list 1)) (list 2))))))

  (testing "contains returns false when a chunked-cons does not contain the item"
    (is (false? (.contains (chunk-cons (make-chunk (list :foo)) nil) :bar))))

  (testing "contains returns true when a chunked-cons does contain the item"
    (is (true? (.contains (chunk-cons (make-chunk (list :foo)) (list :bar)) :bar))))

  (testing "containsAll returns false when the chunked-cons does not contain all of the elements"
    (let [arr-list (ArrayList.)]
      (.add arr-list :bar)
      (.add arr-list :baz)
      (is (false? (.containsAll (chunk-cons (make-chunk (list :foo)) (list :bar)) arr-list)))))

  (testing "containsAll returns true when the chunked-cons does contain all of the elements"
    (let [arr-list (ArrayList.)]
      (.add arr-list :bar)
      (.add arr-list :baz)
      (is (true? (.containsAll (chunk-cons (make-chunk (list :foo)) (list :bar :baz)) arr-list)))))

  (testing "isEmpty returns false for chunked-cons"
    (is (false? (.isEmpty (chunk-cons (make-chunk (list :foo)) nil)))))

  (testing "remove is not supported on chunked-cons"
    (is (thrown? UnsupportedOperationException (.remove (chunk-cons (make-chunk (list 1)) (list 2)) 1))))

  (testing "removeAll is not supported on chunked-cons"
    (is (thrown? UnsupportedOperationException (.removeAll (chunk-cons (make-chunk (list 1)) (list 2)) (ArrayList.)))))

  (testing "retainAll is not supported on chunked-cons"
    (is (thrown? UnsupportedOperationException (.retainAll (chunk-cons (make-chunk (list 1)) (list 2)) (ArrayList.)))))

  (testing "size for chunked-cons"
    (is (= 1 (.size (chunk-cons (make-chunk (list 1)) nil))))
    (is (= 2 (.size (chunk-cons (make-chunk (list 1)) (list 2)))))
    (is (= 3 (.size (chunk-cons (make-chunk (list 1 2)) (list 3))))))

  (testing "toArray with no arguments returns an array containing all elements"
    (let [arr (.toArray (chunk-cons (make-chunk (list :bar)) (list :baz)))]
      (is (= 2 (count arr)))
      (is (= (list :bar) (first arr)))
      (is (= :baz (second arr)))
      (is (= "class [Ljava.lang.Object;" (str (type (.toArray (list :bar :baz)))))))))

