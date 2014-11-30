(ns clojure.lang.persistent-vector-test
  (:refer-clojure :only [defn defmacro apply list list* let nil? re-pattern range])
  (:require [clojure.test                     :refer :all]
            [clojure.lang.platform.exceptions :refer [argument-error out-of-bounds-exception]]
            [clojure.next                     :refer :all]))

(defmacro argument-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? argument-error msg body)))

(defmacro out-of-bounds-exception-is-thrown? [& body]
  (list 'is (list* 'thrown? out-of-bounds-exception body)))

(deftest vector-test
  (testing "returns the count"
    (let [zero (vector)
          one (vector 1)
          two (vector 1 2)
          more (apply vector (range 100))]
      (is (= 0 (count zero)))
      (is (= 1 (count one)))
      (is (= 2 (count two)))
      (is (= 100 (count more)))))

  (testing "returns the nth object"
    (let [new-vec (vector :a :b :c :d :e :f)]
      (is (= :a (nth new-vec 0)))
      (is (= :c (nth new-vec 2)))
      (is (= :f (nth new-vec 5)))))

  (testing "returns the nth object or not-found"
    (let [new-vec (vector :a :b :c :d :e :f)]
      (is (= :a (nth new-vec 0)))
      (is (= :c (nth new-vec 2)))
      (is (= :not-found (nth new-vec 8 :not-found)))))

  (testing "cons"
    (let [new-vec (vector 1 2 3)
          cons-vec (cons 4 new-vec)
          next-cons (cons 5 cons-vec)]
      (is (= 4 (first cons-vec)))
      (is (= 5 (first next-cons)))))

  (testing "assoc the index key with the value"
    (let [new-vec (vector 1 2 3)
          assoc-vec (assoc new-vec 0 :a)
          assoc-vec (assoc assoc-vec 2 :b)]
      (is (= :a (nth assoc-vec 0)))
      (is (= :b (nth assoc-vec 2)))))

  (testing "throws argument error if key is not an integer"
    (let [new-vec (vector 1 2 3)]
      (argument-error-is-thrown? (re-pattern "Key must be an integer")
        (assoc new-vec :a :b))))

  (testing "throws out of bounds exception if index is out of bounds"
    (let [new-vec (vector 1 2 3)]
      (out-of-bounds-exception-is-thrown?
        (assoc new-vec 5 :b))))

  (testing "contains-key"
    (let [new-vec (vector :a :b :c :d)]
      (is (contains-key? new-vec 3))
      (is (not (contains-key? new-vec 5)))))
)

(deftest vector-seq-test
  (testing "seq returns nil when empty"
    (is (nil? (seq (vector)))))

  (testing "returns the first item"
    (let [v-seq (seq (vector 3 2 1))]
      (is (= 3 (first v-seq)))))

  (testing "returns next until empty"
    (let [v1-seq (seq (vector 3 2 1))
          v2-seq (next v1-seq)
          v3-seq (next v2-seq)
          v4-seq (next v3-seq)]
      (is (= 2 (first v2-seq)))
      (is (= 1 (first v3-seq)))
      (is (nil? v4-seq))))

  (testing "returns rest until empty"
    (let [v1-seq (seq (vector 3 2 1))
          v2-seq (rest v1-seq)
          v3-seq (rest v2-seq)
          v4-seq (rest v3-seq)]
      (is (= 2 (first v2-seq)))
      (is (= 1 (first v3-seq)))
      (is (empty? v4-seq))))

  (testing "returns the count"
    (let [v1-seq (seq (vector 3 2 1))
          v2-seq (next v1-seq)
          v3-seq (next v2-seq)]
      (is (= 3 (count v1-seq)))
      (is (= 2 (count v2-seq)))
      (is (= 1 (count v3-seq)))))

  (testing "is seqable"
    (let [v-seq (seq (vector :a :b :c))]
      (is (= :a (first (seq v-seq))))))
)
