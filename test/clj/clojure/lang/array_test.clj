(ns clojure.lang.array-test
  (:refer-clojure :only [boolean-array char-array defmacro defn- let map])
  (:require [clojure.test                         :refer :all]
            [clojure.next                         :refer :all]
            [clojure.support.exception-assertions :refer [out-of-bounds-exception-is-thrown?]]
            [clojure.support.test-seq             :refer [test-seq]]))

(deftest aget-and-set-test
  (testing "setting mutates the given array"
    (let [arr (make-array 1)]
      (aset arr 0 :val)
      (is (= :val (aget arr 0)))))

  (testing "throws an exception if setting at index greater than or equal to max size"
    (let [arr (make-array 1)]
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aset arr 1 :val))
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aset arr 2 :val))))

  (testing "throws an exception if setting at index less than zero"
    (let [arr (make-array 1)]
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aset arr -1 :val))))

  (testing "throws an exception if getting at index greater than or equal to max size"
    (let [arr (make-array 1)]
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aget arr 1))
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aget arr 2))))

  (testing "throws an exception if setting at index less than zero"
    (let [arr (make-array 1)]
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aget arr -1))))

  )

(deftest alength-test
  (testing "returns the length of the array"
    (is (= 1 (alength (make-array 1))))
    (is (= 2 (alength (make-array 2))))))

(deftest array-clone-test
  (testing "returns a new array with the same elements and size"
    (let [arr (make-array 2)
          _ (aset arr 0 :one)
          _ (aset arr 1 :two)
          new-arr (aclone arr)]
      (is (= :one (aget new-arr 0)))
      (is (= :two (aget new-arr 1)))
      (is (= 2 (alength new-arr)))
      (is (not (identical? new-arr arr)))))

  (testing "cloning an empty array"
    (let [arr (make-array 0)
          new-arr (aclone arr)]
      (is (= 0 (alength new-arr))))))

(deftest into-array-test
  (testing "makes an array out of seq items"
    (let [seqable '(:a :b :c)
          arr (into-array seqable)]
      (is (= 3 (alength arr)))
      (is (= :a (aget arr 0)))
      (is (= :b (aget arr 1)))
      (is (= :c (aget arr 2)))))

  (testing "makes an array out of an empty seq"
    (let [seqable '()
          arr (into-array seqable)]
      (is (= 0 (alength arr))))))

(deftest object-array-test
  (testing "returns an array of nils for given size"
    (let [arr (object-array 42)]
      (is (= 42 (alength arr)))
      (is (nil? (aget arr 21)))))

  (testing "returns an array from a given seq"
    (let [s (test-seq '(1 2 3))
          arr (object-array s)]
      (is (= 3 (alength arr)))
      (is (= 1 (aget arr 0)))
      (is (= 2 (aget arr 1)))
      (is (= 3 (aget arr 2))))))

(defn- number-array-test [array-fn type-fn]
  (testing (str "returns an array of " type-fn " the for a given size")
    (let [arr (array-fn 42)]
      (is (= 42 (alength arr)))
      (is (= (type-fn 0) (aget arr 21)))))

  (testing "returns an array from a given seq"
    (let [s (test-seq (map #(type-fn %) '(1 2 3)))
          arr (array-fn s)]
      (is (= 3 (alength arr)))
      (is (= (type-fn 1) (aget arr 0)))
      (is (= (type-fn 2) (aget arr 1)))
      (is (= (type-fn 3) (aget arr 2)))))

  (testing "returns an array of a given size where all elements are the same"
    (let [n42 (type-fn 42)
          arr (array-fn 3 n42)]
      (is (= 3 (alength arr)))
      (is (= n42 (aget arr 0)))
      (is (= n42 (aget arr 1)))
      (is (= n42 (aget arr 2)))))

  (testing "returns an array of a given size sourcing from a seq"
    (let [s (test-seq (map #(type-fn %) '(1 2 3)))
          arr (array-fn 3 s)]
      (is (= 3 (alength arr)))
      (is (= (type-fn 1) (aget arr 0)))
      (is (= (type-fn 2) (aget arr 1)))
      (is (= (type-fn 3) (aget arr 2))))))

(deftest byte-array-test
  (number-array-test byte-array byte))

(deftest short-array-test
  (number-array-test short-array short))

(deftest int-array-test
  (number-array-test int-array int))

(deftest long-array-test
  (number-array-test long-array long))

(deftest float-array-test
  (number-array-test float-array float))

(deftest double-array-test
  (number-array-test double-array double))

(deftest booleans-test
  (testing "type casting a boolean array to a boolean array"
    (is (=
          (type (booleans (boolean-array 0)))
          (type (boolean-array 0))))))

(deftest bytes-test
  (testing "type casting a byte array to a byte array"
    (is (=
          (type (bytes (byte-array 0)))
          (type (byte-array 0))))))

(deftest chars-test
  (testing "type casting a char array to a char array"
    (is (=
          (type (chars (char-array 0)))
          (type (char-array 0))))))

(deftest shorts-test
  (testing "type casting a short array to a short array"
    (is (=
          (type (shorts (short-array 0)))
          (type (short-array 0))))))

(deftest floats-test
  (testing "type casting a float array to a float array"
    (is (=
          (type (floats (float-array 0)))
          (type (float-array 0))))))

(deftest doubles-test
  (testing "type casting a double array to a double array"
    (is (=
          (type (doubles (double-array 0)))
          (type (double-array 0))))))

(deftest ints-test
  (testing "type casting an int array to an int array"
    (is (=
          (type (ints (int-array 0)))
          (type (int-array 0))))))

(deftest longs-test
  (testing "type casting a long array to a long array"
    (is (=
          (type (longs (long-array 0)))
          (type (long-array 0))))))

