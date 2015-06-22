(ns clojure.lang.transient-map-test
  (:refer-clojure :only [defn defn- let])
  (:require [clojure.test                         :refer :all]
            [clojure.support.exception-assertions :refer [argument-error-is-thrown? illegal-access-error-is-thrown?]]
            [clojure.next                         :refer :all]))

(defn- transient-map-persistent-test [constructor]
  (testing "count raises an error if the transient has been made persistent"
    (let [t (transient (constructor))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (count t)))))

(defn- transient-map-count-test [constructor]
  (testing "counting a transient array map"
    (let [zero-size (transient (constructor))
          two-size (transient (constructor :a 1 :b 2))]
      (is (= 0 (count zero-size)))
      (is (= 2 (count two-size)))))

  (testing "count raises an error if the transient has been made persistent"
    (let [t (transient (constructor))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (count t)))))

(defn- transient-map-assoc!-test [constructor]
  (testing "assoc! to a transient"
    (let [t (transient (constructor :a 1))]
      (assoc! t :b 2)
      (assoc! t :c 3)
      (let [p (persistent! t)]
        (is (= 3 (count p)))
        (is (= 1 (get p :a)))
        (is (= 2 (get p :b)))
        (is (= 3 (get p :c))))))

  (testing "assoc! to an existing key val pair"
    (let [t (transient (constructor :a 1))]
      (assoc! t :a 2)
      (let [p (persistent! t)]
        (is (= 1 (count p)))
        (is (= 2 (val (first p)))))))

  (testing "assoc! raises an error if the transient has been made persistent"
    (let [t (transient (constructor))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (assoc! t :a 1)))))

(defn- transient-map-conj!-test [constructor]
  (testing "conj! a vector tuple"
    (let [tuple (vector :k :v)
          t (transient (constructor))]
      (conj! t tuple)
      (let [p (persistent! t)]
        (is (= 1 (count p)))
        (is (= :k (key (first p))))
        (is (= :v (val (first p)))))))

  (testing "conj! a vector tuple to an existing key"
    (let [tuple (vector :k :v)
          t (transient (constructor :k :not-v))]
      (conj! t tuple)
      (let [p (persistent! t)]
        (is (= 1 (count p)))
        (is (= :k (key (first p))))
        (is (= :v (val (first p)))))))

  (testing "an argument error is raised when conj! is invoked with a vector tuple not of size 2"
    (let [t (transient (constructor))]
      (argument-error-is-thrown?
        #"Vector arg to map conj must be a pair"
        (conj! t (vector :k)))))

  (testing "conj! raises an error if the transient has been made persistent"
    (let [t (transient (constructor))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (conj! t (vector :k :v))))))

(defn- transient-map-dissoc!-test [constructor]
  (testing "dissoc! a key that is not in the map"
    (let [t (transient (constructor :k :v))]
      (dissoc! t :foo)
      (let [p (persistent! t)]
        (is (= 1 (count p)))
        (is (= :k (key (first p))))
        (is (= :v (val (first p)))))))

  (testing "dissoc! a key that is in the map"
    (let [t (transient (constructor :k :v))]
      (dissoc! t :k)
      (let [p (persistent! t)]
        (is (empty? p)))))

  (testing "dissoc! raises an error if the transient has been made persistent"
    (let [t (transient (constructor))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (dissoc! t :k)))))

(defn- transient-map-get-test [constructor]
  (testing "get a value for a given key"
    (let [t (transient (constructor :k :v))]
      (is (= :v (get t :k)))))

  (testing "get returns nil when the key is not found and a not found is not specified"
    (let [t (transient (constructor :k :v))]
      (is (nil? (get t :foo)))))

  (testing "get returns the not found value when not found"
    (let [t (transient (constructor :k :v))]
      (is (= :sentinel (get t :foo :sentinel)))))

  (testing "get raises an error if the transient has been made persistent"
    (let [t (transient (constructor))]
      (persistent! t)
      (illegal-access-error-is-thrown?
        #"Transient used after persistent! call"
        (get t :k)))))

(defn transient-map-test [constructor]
  (transient-map-persistent-test constructor)
  (transient-map-count-test constructor)
  (transient-map-assoc!-test constructor)
  (transient-map-conj!-test constructor)
  (transient-map-dissoc!-test constructor)
  (transient-map-get-test constructor))
