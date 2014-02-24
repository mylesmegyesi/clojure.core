(ns clojure.lang.array-test
  (:refer-clojure :only [defmacro let list list*])
  (:require [clojure.test                     :refer :all]
            [clojure.lang.platform.exceptions :refer [out-of-bounds-exception]]
            [clojure.next                     :refer :all]))

(defmacro out-of-bounds-exeption-is-thrown? [& body]
  (list 'is (list* 'thrown? out-of-bounds-exception body)))

(deftest aget-and-set-test
  (testing "setting mutates the given array"
    (let [arr (make-array 1)]
      (aset arr 0 :val)
      (is (= :val (aget arr 0)))))

  (testing "throws an exception if setting at index greater than or equal to max size"
    (let [arr (make-array 1)]
      (out-of-bounds-exeption-is-thrown?
        (aset arr 1 :val))
      (out-of-bounds-exeption-is-thrown?
        (aset arr 2 :val))))

  (testing "throws an exception if setting at index less than zero"
    (let [arr (make-array 1)]
      (out-of-bounds-exeption-is-thrown?
        (aset arr -1 :val))))

  (testing "throws an exception if getting at index greater than or equal to max size"
    (let [arr (make-array 1)]
      (out-of-bounds-exeption-is-thrown?
        (aget arr 1))
      (out-of-bounds-exeption-is-thrown?
        (aget arr 2))))

  (testing "throws an exception if setting at index less than zero"
    (let [arr (make-array 1)]
      (out-of-bounds-exeption-is-thrown?
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
