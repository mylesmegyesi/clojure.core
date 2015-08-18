(ns clojure.lang.persistent-vector-test
  (:refer-clojure :only [apply defn defmacro doseq fn for let range reify re-pattern])
  (:require [clojure.test                         :refer :all]
            [clojure.lang.protocols               :refer [IPersistentVector]]
            [clojure.support.exception-assertions :refer :all]
            [clojure.next                         :refer :all]))

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

  (testing "generate an empty vector"
    (let [v (vector 1 2 3)]
      (is (empty? (empty v)))))

  (testing "empty vector retains the meta"
    (let [mta {:so :meta}
          v (with-meta (vector 1 2 3) mta)]
      (is (= mta (meta (empty v))))))

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

  (testing "contains?"
    (let [new-vec (vector :a :b :c :d)]
      (is (contains? new-vec 3))
      (is (not (contains? new-vec 5)))))

  (testing "peek"
    (is (nil? (peek (vector))))
    (is (= 3 (peek (vector 1 2 3)))))

  (testing "popping an empty vector throwns an exception"
    (illegal-state-error-is-thrown?
      #"Can't pop empty vector"
      (pop (vector))))

  (testing "popping a single element vector preserves the meta"
    (let [v (with-meta (vector 1) {:so :meta})
          empty-v (pop v)]
      (is (zero? (count empty-v)))
      (is (= {:so :meta} (meta empty-v)))))

  (testing "popping many elements off of a vector"
    (let [fifty-foos (clojure.core/repeat 50 :foo)
          v (apply vector fifty-foos)]
      (let [result (reduce (fn [acc _] (pop acc)) v fifty-foos)]
        (is (empty? result))))))

(deftest vector?-test
  (testing "returns true for a vector"
    (is (vector? (vector))))

  (testing "returns true for any implementer of IPersistentVector"
    (is (vector? (reify IPersistentVector))))

  (testing "returns false otherwise"
    (is (not (vector? :foo)))))

(deftest chunked-seq-test
  (testing "seq returns nil when empty"
    (is (nil? (seq (vector)))))

  (testing "returns the first item"
    (let [v-seq (seq (vector 3 2 1))]
      (is (= 3 (first v-seq)))))

  (testing "returns the chunk first"
    (let [c-first (chunk-first (seq (vector 3 2 1)))]
      (is (= 3 (count c-first)))
      (is (= 3 (nth c-first 0)))))

  (testing "returns the chunk next"
    (let [v (apply vector (range 33))
          c-next (chunk-next (seq v))]
      (is (= 32 (nth c-next 0)))))

  (testing "returns nil when there is no chunk next"
    (let [c-next (chunk-next (seq (vector 3)))]
      (is (nil? c-next))))

  (testing "returns the chunk more"
    (let [v (apply vector (range 33))
          c-more (chunk-rest (seq v))]
      (is (= 32 (nth c-more 0)))))

  (testing "returns an empty list when there is no chunk more"
    (let [c-more (chunk-rest (seq (vector 3)))]
      (is (list? c-more))
      (is (empty? c-more))))

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

  (testing "can hold a meta value"
    (let [v-seq (seq (vector :a :b :C))
          w-meta (with-meta v-seq {:so :meta})]
      (is (= {:so :meta} (meta w-meta)))))
  )

(deftest chunked-seq-test?
  (testing "is true for a chunked seq"
    (is (chunked-seq? (seq (vector 1)))))

  (testing "is false otherwise"
    (is (not (chunked-seq? 1)))))

(deftest transient-vector-test
  (testing "returns the count"
    (let [size3-transient (transient (vector 3 2 1))
          size0-transient (transient (vector))]
      (is (= 3 (count size3-transient)))
      (is (= 0 (count size0-transient)))))

  (testing "count throws an exception after the transient has become persistent"
    (let [t (transient (vector))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (count t))))

  (testing "nth of a transient vector"
    (let [t (transient (vector 0 1 2))]
      (is (= 1 (nth t 1)))))

  (testing "nth throws an out of bounds exception without a not-found value"
    (let [t (transient (vector 0 1 2))]
      (out-of-bounds-exception-is-thrown?
        #".*"
        (nth t 99))))

  (testing "nth returns the default value when provided and n is out of bounds"
    (let [t (transient (vector 0 1 2))]
      (is (= :foo (nth t 99 :foo)))))

  (testing "nth throws an exception after the transient has become persistent without a default"
    (let [t (transient (vector))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (nth t 0))
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (nth t 0 :foo))))

  (testing "conj! onto a transient"
    (let [t (transient (vector))]
      (conj! t :first)
      (conj! t :second)
      (let [result (persistent! t)]
        (is (= 2 (count result)))
        (is (= :first (first result)))
        (is (= :second (second result))))))

  (testing "conj! over 32 values"
    (let [t (transient (vector))
          r (range 0 33)]
      (doseq [value r]
        (conj! t value))
      (let [result (persistent! t)]
        (for [index r]
          (is (= (nth index result) (clojure.core/nth index r)))))))

  (testing "conj! throws an exception after the transient has become persistent"
    (let [t (transient (vector))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (conj! t :foo))))

  (testing "assoc! conjs when associng the last index"
    (let [t (transient (vector))]
      (assoc! t 0 :first)
      (assoc! t 1 :second)
      (let [result (persistent! t)]
        (is (=  2 (count result)))
        (is (= :first (first result)))
        (is (= :second (second result))))))

  (testing "assoc! sets a vector index"
    (let [t (transient (vector 1 2 3))]
      (assoc! t 1 :two)
      (let [result (persistent! t)]
        (is (= :two (second result))))))

  (testing "assoc! raises an exception when the index is out of bounds"
    (let [t (transient (vector))]
      (out-of-bounds-exception-is-thrown?
        (assoc! t -1 :foo))
      (out-of-bounds-exception-is-thrown?
        (assoc! t 1 :foo))))

  (testing "assoc! over 32 values"
    (let [t (transient (vector))
          r (range 0 33)]
      (doseq [value r]
        (assoc! t value value))
      (let [result (persistent! t)]
        (for [index r]
          (is (= nth index result) (clojure.core/nth index r))))))

  (testing "assoc! throwns an exception after the transient has become persistent"
    (let [t (transient (vector))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (assoc! t 0 :foo))))

  (testing "assoc! throws an exception if the key is not an integer"
    (let [t (transient (vector))]
      (argument-error-is-thrown?
        #"Key must be integer"
        (assoc! t :foo :bar))))

  (testing "pop! throws an exception if there are no elements"
    (let [t (transient (vector))]
      (illegal-state-error-is-thrown?
        #"Can't pop empty vector"
        (pop! t))))

  (testing "pop! the last value"
    (let [t (transient (vector 1))]
      (pop! t)
      (let [result (persistent! t)]
        (is (zero? (count result))))))

  (testing "pop! a few values"
    (let [t (transient (vector 1 2 3 4 5))]
      (pop! t)
      (pop! t)
      (let [result (persistent! t)]
        (is (= 3 (count result))))))

  (testing "pop! over 32 values"
    (let [t (transient (vector))
          r (range 0 33)]
      (doseq [value r]
        (assoc! t value value))
      (doseq [_ r]
        (pop! t))
      (let [result (persistent! t)]
        (is (zero? (count result))))))

  )
