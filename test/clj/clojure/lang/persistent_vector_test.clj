(ns clojure.lang.persistent-vector-test
  (:refer-clojure :only [apply defn defmacro doseq for list list* let nil? range re-pattern])
  (:require [clojure.test            :refer :all]
            [clojure.lang.exceptions :refer [argument-error illegal-access-error
                                             illegal-state-error out-of-bounds-exception]]
            [clojure.next            :refer :all]))

(defmacro argument-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? argument-error msg body)))

(defmacro illegal-access-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? illegal-access-error msg body)))

(defmacro illegal-state-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? illegal-state-error msg body)))

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
