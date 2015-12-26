(ns clojure.lang.array-test
  (:refer-clojure :only [cond defmacro defn- let])
  (:require [clojure.test                         :refer :all]
            [clojure.next                         :refer :all]
            [clojure.lang.object                  :as    obj]
            [clojure.lang.numbers                 :as    numbers]
            [clojure.lang.primitive-array         :as    prim-arr]
            [clojure.support.exception-assertions :refer [argument-error-is-thrown?
                                                          class-cast-exception-is-thrown?
                                                          out-of-bounds-exception-is-thrown?]]
            [clojure.support.test-seq             :refer [test-seq]]))

(deftest aget-and-set-test
  (testing "setting mutates the given array"
    (let [arr (object-array 1)]
      (aset arr 0 :val)
      (is (= :val (aget arr 0)))))

  (testing "throws an exception if setting at index greater than or equal to max size"
    (let [arr (object-array 1)]
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aset arr 1 :val))
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aset arr 2 :val))))

  (testing "throws an exception if setting at index less than zero"
    (let [arr (object-array 1)]
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aset arr -1 :val))))

  (testing "throws an exception if getting at index greater than or equal to max size"
    (let [arr (object-array 1)]
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aget arr 1))
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aget arr 2))))

  (testing "throws an exception if setting at index less than zero"
    (let [arr (object-array 1)]
      (out-of-bounds-exception-is-thrown?
        #".*"
        (aget arr -1)))))

(defn- aset-type-test [aset-fn t v]
  (testing (str "set a " t " value in an array")
    (let [arr (make-array t 1)]
      (aset-fn arr 0 v)
      (is (= v (aget arr 0)))))

  (testing (str "an illegal argument error is thrown if the array is not a " t " array")
    (let [arr (object-array 1)]
      (argument-error-is-thrown? #"" (aset-fn arr 0 v))))

  (testing (str "a class cast exception is thrown if the arg is not castable to an " t)
    (let [arr (make-array t 1)]
      (class-cast-exception-is-thrown? #"" (aset-fn arr 0 "foo"))))

  (testing (str "a value can be set in a nested " t " array")
    (let [arr (make-array t 1 1)]
      (aset-fn arr 0 0 v)
      (is (= v (aget (aget arr 0) 0))))))

(deftest aset-byte-test
  (aset-type-test aset-byte numbers/platform-native-byte 42))

(deftest aset-short-test
  (aset-type-test aset-short numbers/platform-native-short 42))

(deftest aset-int-test
  (aset-type-test aset-int numbers/platform-native-int 42))

(deftest aset-long-test
  (aset-type-test aset-long numbers/platform-native-long 42))

(deftest aset-float-test
  (aset-type-test aset-float numbers/platform-native-float 42.0))

(deftest aset-double-test
  (aset-type-test aset-double numbers/platform-native-double 42.0))

; The boolean cast method always returns a boolean
; so we must skip the class cast exception test
(deftest aset-boolean-test
  (testing "set a boolean value in an array"
    (let [arr (make-array prim-arr/platform-native-boolean 1)]
      (aset-boolean arr 0 true)
      (is (= true (aget arr 0)))))

  (testing "an illegal argument error is thrown if the array is not a boolean array"
    (let [arr (object-array 1)]
      (argument-error-is-thrown? #"" (aset-boolean 0 true))))

  (testing "a value can be set in a nested boolean array"
    (let [arr (make-array prim-arr/platform-native-boolean 1 1)]
      (aset-boolean arr 0 0 true)
      (is (= true (aget (aget arr 0) 0))))))

(deftest alength-test
  (testing "returns the length of the array"
    (is (= 1 (alength (object-array 1))))
    (is (= 2 (alength (object-array 2))))))

(deftest array-clone-test
  (testing "returns a new array with the same elements and size"
    (let [arr (object-array 2)
          _ (aset arr 0 :one)
          _ (aset arr 1 :two)
          new-arr (aclone arr)]
      (is (= :one (aget new-arr 0)))
      (is (= :two (aget new-arr 1)))
      (is (= 2 (alength new-arr)))
      (is (not (identical? new-arr arr)))))

  (testing "cloning an empty array"
    (let [arr (object-array 0)
          new-arr (aclone arr)]
      (is (= 0 (alength new-arr))))))

(deftest make-array-test
  (testing "creates an array for a type of a certain length"
    (let [arr (make-array obj/base-object 1)]
      (is (= 1 (alength arr)))))

  (testing "creates a nested array for a type of a certain length"
    (let [arr (make-array obj/base-object 1 1)]
      (is (= 1 (alength (aget arr 0)))))))

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

(deftest amap-test
  (testing "map over array elements with an index reference and return reference"
    (let [seqable '(1 2 3)
          arr (into-array seqable)
          ret (amap arr idx ret
                    (cond
                      (= idx 0) (inc (aget arr 0))
                      (= idx 1) (+ 2 (aget ret 0))
                      (= idx 2) (- (aget arr 2) 5)))]
      (is (= 1 (aget arr 0)))
      (is (= 2 (aget arr 1)))
      (is (= 3 (aget arr 2)))
      (is (= 2 (aget ret 0)))
      (is (= 4 (aget ret 1)))
      (is (= -2 (aget ret 2))))))

(deftest areduce-test
  (testing "reduce over array elements with an index reference and return reference"
    (let [seqable '(1 2 3)
          arr (into-array seqable)
          ret (areduce arr idx ret 42
                       (cond
                         (even? idx) (+ ret (inc (aget arr idx)))
                         (odd? idx) (* ret 2)))]
      (is (= 1 (aget arr 0)))
      (is (= 2 (aget arr 1)))
      (is (= 3 (aget arr 2)))
      (is (= 92 ret)))))

(deftest to-array-test
  (testing "passing nil returns a zero argument object array"
    (is (= (type (to-array nil)) (type (object-array 0))))
    (is (zero? (count (to-array nil))))))

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
  (testing (str "returns an array of " type-fn " the for a given size filled with zero values")
    (let [arr (array-fn 42)]
      (is (= 42 (alength arr)))
      (is (= (type-fn 0) (aget arr 21)))))

  (testing "returns an array from a given seq"
    (let [s (map #(type-fn %) (vector 1 2 3))
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
    (let [s (map #(type-fn %) (vector 1 2 3 4 5))
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

(deftest boolean-array-test
  (testing "returns an array of booleans the for a given size filled with false values"
    (let [arr (boolean-array 42)]
      (is (= 42 (alength arr)))
      (is (= false (aget arr 21)))))

  (testing "returns an array from a given seq"
    (let [s (test-seq '(true false true))
          arr (boolean-array s)]
      (is (= 3 (alength arr)))
      (is (= true (aget arr 0)))
      (is (= false (aget arr 1)))
      (is (= true (aget arr 2)))))

  (testing "returns an array of a given size where all elements are the same"
    (let [arr (boolean-array 3 true)]
      (is (= 3 (alength arr)))
      (is (= true (aget arr 0)))
      (is (= true (aget arr 1)))
      (is (= true (aget arr 2)))))

  (testing "returns an array of a given size sourcing from a seq"
    (let [s (test-seq '(true false true false true))
          arr (boolean-array 3 s)]
      (is (= 3 (alength arr)))
      (is (= true (aget arr 0)))
      (is (= false (aget arr 1)))
      (is (= true (aget arr 2))))))

(deftest char-array-test
  (testing "returns an array of chars the for a given size filled with the 0-ascii character"
    (let [arr (char-array 42)]
      (is (= 42 (alength arr)))
      (is (= (char 0) (aget arr 21)))))

  (testing "returns an array from a given seq"
    (let [s (test-seq '(\b \a \r))
          arr (char-array s)]
      (is (= 3 (alength arr)))
      (is (= \b (aget arr 0)))
      (is (= \a (aget arr 1)))
      (is (= \r (aget arr 2)))))

  (testing "returns an array of a given size where all elements are the same"
    (let [arr (char-array 3 \p)]
      (is (= 3 (alength arr)))
      (is (= \p (aget arr 0)))
      (is (= \p (aget arr 1)))
      (is (= \p (aget arr 2)))))

  (testing "returns an array of a given size sourcing from a seq"
    (let [s (test-seq '(\b \a \r \f \o \o))
          arr (char-array 3 s)]
      (is (= 3 (alength arr)))
      (is (= \b (aget arr 0)))
      (is (= \a (aget arr 1)))
      (is (= \r (aget arr 2))))))

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

