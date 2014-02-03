(ns clojure.lang.array-test
  (:refer-clojure :only [defmacro let list list*])
  (:require [clojure.test            :refer :all]
            [clojure.lang.array      :refer [make-array array-get array-set!
                                             array-length array-clone! into-array]]
            [clojure.lang.operators  :refer [= not]]
            [clojure.lang.exceptions :refer [out-of-bounds-exception]]
            [clojure.lang.object     :refer [identical?]]))

(defmacro out-of-bounds-exeption-is-thrown? [& body]
  (list 'is (list* 'thrown? out-of-bounds-exception body)))

(deftest array-get-and-set-test
  (testing "setting mutates the given array"
    (let [arr (make-array 1)]
      (array-set! arr 0 :val)
      (is (= :val (array-get arr 0)))))

  (testing "throws an exception if setting at index greater than or equal to max size"
    (let [arr (make-array 1)]
      (out-of-bounds-exeption-is-thrown?
        (array-set! arr 1 :val))
      (out-of-bounds-exeption-is-thrown?
        (array-set! arr 2 :val))))

  (testing "throws an exception if setting at index less than zero"
    (let [arr (make-array 1)]
      (out-of-bounds-exeption-is-thrown?
        (array-set! arr -1 :val))))

  (testing "throws an exception if getting at index greater than or equal to max size"
    (let [arr (make-array 1)]
      (out-of-bounds-exeption-is-thrown?
        (array-get arr 1))
      (out-of-bounds-exeption-is-thrown?
        (array-get arr 2))))

  (testing "throws an exception if setting at index less than zero"
    (let [arr (make-array 1)]
      (out-of-bounds-exeption-is-thrown?
        (array-get arr -1))))

  )

(deftest array-length-test
  (testing "returns the length of the array"
    (is (= 1 (array-length (make-array 1))))
    (is (= 2 (array-length (make-array 2))))))

(deftest array-clone-test
  (testing "returns a new array with the same elements and size"
    (let [arr (make-array 2)
          _ (array-set! arr 0 :one)
          _ (array-set! arr 1 :two)
          new-arr (array-clone! arr)]
      (is (= :one (array-get new-arr 0)))
      (is (= :two (array-get new-arr 1)))
      (is (= 2 (array-length new-arr)))
      (is (not (identical? new-arr arr)))))

  (testing "cloning an empty array"
    (let [arr (make-array 0)
          new-arr (array-clone! arr)]
      (is (= 0 (array-length new-arr))))))

(deftest into-array-test
  (testing "makes an array out of seq items"
    (let [seqable '(:a :b :c)
          arr (into-array seqable)]
      (is (= 3 (array-length arr)))
      (is (= :a (array-get arr 0)))
      (is (= :b (array-get arr 1)))
      (is (= :c (array-get arr 2)))))

  (testing "makes an array out of an empty seq"
    (let [seqable '()
          arr (into-array seqable)]
      (is (= 0 (array-length arr))))))
