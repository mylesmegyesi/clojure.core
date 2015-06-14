(ns clojure.lang.persistent-array-map-test
  (:refer-clojure :only [defmacro deftype let list list*])
  (:require [clojure.test                     :refer :all]
            [clojure.lang.exceptions          :refer [argument-error illegal-access-error]]
            [clojure.lang.persistent-map-test :refer [map-test]]
            [clojure.lang.protocols           :refer [ISeqable ISequential]]
            [clojure.lang.persistent-list     :refer [EMPTY-LIST]]
            [clojure.next                     :refer :all]))

(defmacro argument-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? argument-error msg body)))

(defmacro illegal-access-error-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? illegal-access-error msg body)))

(deftype FixedSequential [seq]
  ISequential
  ISeqable
  (-seq [this] seq))

(deftest array-map-test
  (map-test "PersistentArrayMap" array-map))

(deftest array-map-seq-test
  (testing "seq returns nil when the map is empty"
    (is (nil? (seq (array-map)))))

  (testing "first gives a map entry with the key and val"
    (let [m1 (array-map :k1 1)
          m1-seq (seq m1)
          entry (first m1-seq)]
      (is (= :k1 (key entry)))
      (is (= 1 (val entry)))))

  (testing "next returns nil when there is only one entry"
    (is (nil? (next (seq (array-map :k1 1))))))

  (testing "rest returns an empty list when there is only one entry"
    (is (= EMPTY-LIST (rest (seq (array-map :k1 1))))))

  (testing "returns items in the order that they are constructed"
    (let [m1 (array-map :k1 1 :k2 2)
          m1-seq (seq m1)
          entry (first m1-seq)]
      (is (= :k1 (key entry)))
      (is (= 1 (val entry)))))

  (testing "next returns nil when there more than one entry"
    (let [m1 (array-map :k1 1 :k2 2)
          m1-seq (seq m1)
          m2-seq (next m1-seq)
          entry (first m2-seq)]
      (not (identical? m1-seq m2-seq))
      (is (= :k2 (key entry)))
      (is (= 2 (val entry)))))

  (testing "can keep calling next and eventually get to every item"
    (let [m1-seq (seq (array-map :k1 1 :k2 2 :k3 3 :k4 4 :k5 5))
          m2-seq (next m1-seq)
          m3-seq (next m2-seq)
          m4-seq (next m3-seq)
          m5-seq (next m4-seq)
          m6-seq (next m5-seq)]
      (is (= :k1 (key (first m1-seq))))
      (is (= 1   (val (first m1-seq))))
      (is (= :k2 (key (first m2-seq))))
      (is (= 2   (val (first m2-seq))))
      (is (= :k3 (key (first m3-seq))))
      (is (= 3   (val (first m3-seq))))
      (is (= :k4 (key (first m4-seq))))
      (is (= 4   (val (first m4-seq))))
      (is (= :k5 (key (first m5-seq))))
      (is (= 5   (val (first m5-seq))))
      (is (nil? m6-seq))))

  (testing "counts the items in a seq"
    (let [m1-seq (seq (array-map :k1 1 :k2 2 :k3 3 :k4 4 :k5 5))
          m2-seq (next m1-seq)
          m3-seq (next m2-seq)
          m4-seq (next m3-seq)
          m5-seq (next m4-seq)
          m6-seq (next m5-seq)]
      (is (= 5 (count m1-seq)))
      (is (= 4 (count m2-seq)))
      (is (= 3 (count m3-seq)))
      (is (= 2 (count m4-seq)))
      (is (= 1 (count m5-seq)))))

  (testing "array-map-seqs and Seqable and return themselves"
    (let [s (seq (array-map :k1 1 :k2 2 :k3 3))]
      (is (identical? s (seq s)))))

  (testing "seqs are equal when every seq entry is equal"
    (is (= (seq (array-map :k1 1 :k2 2 :k3 3))
           (seq (array-map :k1 1 :k2 2 :k3 3)))))

  (testing "seqs are not equal when one item is missing from other"
    (is (not= (seq (array-map :k1 1 :k2 2 :k3 3))
              (seq (array-map :k1 1 :k2 2)))))

  (testing "seqs are not equal when one item is missing from this"
    (is (not= (seq (array-map :k1 1 :k2 2))
              (seq (array-map :k1 1 :k2 2 :k3 3)))))

  (testing "seqs are not equal when other is not a sequential"
    (is (not= (seq (array-map :k1 1)) 1)))

  (testing "seqs are equal to sequential things"
    (let [s1 (seq (array-map :k1 1 :k2 2))
          s2 (seq (array-map :k1 1 :k2 2))]
      (is (= s1 (FixedSequential. s2))))))

(deftest transient-array-map
  (testing "meta is preserved"
    (let [m (with-meta (array-map) {:so :meta})
          t (persistent! (transient m))]
      (is (= {:so :meta} (meta t)))))

  (testing "persistent! can only be invoked once"
    (let [t (transient (array-map))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (persistent! t))))

  (testing "counting a transient array map"
    (let [zero-size (transient (array-map))
          two-size (transient (array-map :a 1 :b 2))]
      (is (= 0 (count zero-size)))
      (is (= 2 (count two-size)))))

  (testing "count raises an error if the transient has been made persistent"
    (let [t (transient (array-map))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (count t))))

  (testing "assoc! to a transient"
    (let [t (transient (array-map :a 1))]
      (assoc! t :b 2)
      (assoc! t :c 3)
      (let [p (persistent! t)]
        (is (= 3 (count p)))
        (is (= :a (key (first p))))
        (is (= :b (key (first (rest p)))))
        (is (= :c (key (first (rest (rest p)))))))))

  (testing "assoc! to an existing key val pair"
    (let [t (transient (array-map :a 1))]
      (assoc! t :a 2)
      (let [p (persistent! t)]
        (is (= 1 (count p)))
        (is (= 2 (val (first p)))))))

  (testing "assoc! raises an error if the transient has been made persistent"
    (let [t (transient (array-map))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (assoc! t :a 1))))

  (testing "conj! a vector tuple"
    (let [tuple (vector :k :v)
          t (transient (array-map))]
      (conj! t tuple)
      (let [p (persistent! t)]
        (is (= 1 (count p)))
        (is (= :k (key (first p))))
        (is (= :v (val (first p)))))))

  (testing "conj! a vector tuple to an existing key"
    (let [tuple (vector :k :v)
          t (transient (array-map :k :not-v))]
      (conj! t tuple)
      (let [p (persistent! t)]
        (is (= 1 (count p)))
        (is (= :k (key (first p))))
        (is (= :v (val (first p)))))))

  (testing "an argument error is raised when conj! is invoked with a vector tuple not of size 2"
    (let [t (transient (array-map))]
      (argument-error-is-thrown?
        #"Vector arg to map conj must be a pair"
        (conj! t (vector :k)))))

  )

