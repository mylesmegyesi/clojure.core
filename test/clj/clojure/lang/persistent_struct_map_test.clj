(ns clojure.lang.persistent-struct-map-test
  (:refer-clojure :only [doseq let])
  (:require [clojure.test                         :refer :all]
            [clojure.lang.protocols               :refer [-get-keys -get-keyslots]]
            [clojure.support.exception-assertions :refer [argument-error-is-thrown?]]
            [clojure.lang.persistent-list         :refer [EMPTY-LIST]]
            [clojure.lang.persistent-struct-map   :refer [make-def]]
            [clojure.next                         :refer :all]))

(deftest def-test
  (testing "new-def raises an argument error if the keys are nil"
    (argument-error-is-thrown? #"Must supply keys" (make-def nil)))

  (testing "keys are equal to the argument"
    (let [d (make-def [:k1 :k2 :k3])]
      (is (= [:k1 :k2 :k3] (-get-keys d)))))

  (testing "keyslots interleave 1 to n for each key"
    (let [d (make-def [:k1 :k2 :k3])]
      (is (= (array-map :k1 1 :k2 2 :k3 3) (-get-keyslots d))))))

(defstruct d :k1 :k2 :k3 :k4 :k5)

(deftest struct-test
  (testing "raises an error if too many keys are passed"
    (argument-error-is-thrown? #"Too many arguments to struct constructor" (struct d 1 2 3 4 5 6)))

  (testing "count is always equal to the number of struct keys"
    (is (= 5 (count (struct d))))
    (is (= 5 (count (struct d 1 2 3))))
    (is (= 5 (count (struct d 1 2 3 4 5)))))

  (testing "get for struct keys"
    (is (= :v2 (get (struct d :v1 :v2 :v3) :k2))))

  (testing "assoc a struct slot"
    (let [s (assoc (struct d) :k1 :v1)]
      (is (= (count s) 5))
      (is (= (get s :k1) :v1))))

  (testing "assoc to an extension"
    (let [s (assoc (struct d) :foo :bar)]
      (is (= (count s) 6))
      (is (= (get s :foo) :bar))))

  (testing "contains? returns true for all struct keys"
    (let [s (struct d)]
      (doseq [k '(:k1 :k2 :k3 :k4 :k5)]
        (is (true? (contains? s k))))))

  (testing "contains? returns true for an extension key"
    (let [s (assoc (struct d) :foo :bar)]
      (is (true? (contains? s :foo)))))

  (testing "cannot dissoc a struct key"
    (let [s0 (struct d :v1)
          s1 (dissoc s0 :k1)]
      (is (true? (contains? s1 :k1)))
      (is (= :v1 (get s1 :k1)))))

  (testing "returning an empty struct map"
    (let [s0 (struct d :k1 :k2 :k3 :k4 :k5)
          s1 (assoc s0 :k6 :v6)
          s2 (empty s1)]
      (is (nil? (get s2 :k1)))
      (is (nil? (get s2 :k6)))))

  (testing "conj a struct map"
    (let [c (conj (struct d) (vector :k6 :v6))]
      (is (= :v6 (get c :k6)))))

  (testing "struct maps can hold a meta value"
    (let [s (with-meta (struct d) (array-map :so :meta))]
      (is (= (array-map :so :meta) (meta s))))))

(deftest struct-map-test
  (testing "throws an exception is there is an uneven amount of keypairs"
    (argument-error-is-thrown? #"No value supplied for key: :k" (struct-map d :k1)))

  (testing "allows struct keys to be set"
    (let [s (struct-map d :k1 :v1)]
      (is (= :v1 (get s :k1)))))

  (testing "allows extension keys to be set"
    (let [s (struct-map d :ext1 :vext1)]
      (is (= :vext1 (get s :ext1))))))

(deftest struct-map-seq-test
  (testing "first is the first key slot"
    (let [sq (seq (struct d :v1 :v2))]
      (is (= :k1 (key (first sq))))
      (is (= :v1 (val (first sq))))))

  (testing "next returns a seq with the rest of the slots"
    (let [sq (next (struct d :v1 :v2))]
      (is (= :k2 (key (first sq))))
      (is (= :v2 (val (first sq))))))

  (testing "seq of ext map is used after struct keyslots"
    (let [sq0 (seq (struct-map d :ext1 :vext1))
          sq1 (next (next (next (next (next sq0)))))]
      (is (= :ext1 (key (first sq1))))
      (is (= :vext1 (val (first sq1))))))

  (testing "rest returns an empty list if there are no most seq elements"
    (let [sq (rest (rest (rest (rest (rest (seq (struct-map d)))))))]
      (is (identical? EMPTY-LIST sq))))

  (testing "seq count for a struct without an ext equals the number of struct keys"
    (is (= 5 (count (seq (struct d))))))

  (testing "seq count accounts for ext"
    (is (= 6 (count (seq (struct-map d :ext :vext))))))

  (testing "struct map seqs can hold a meta value"
    (let [sq (with-meta (seq (struct d)) (array-map :so :meta))]
      (is (= (array-map :so :meta) (meta sq))))))
