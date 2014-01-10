(ns clojure.lang.keyword-test
  (:refer-clojure :only [let + defmacro])
  (:require [clojure.test                 :refer :all]
            [clojure.lang.assertions      :refer :all]
            [clojure.lang.hash            :refer [hash]]
            [clojure.lang.keyword         :refer :all]
            [clojure.lang.meta            :refer [meta with-meta]]
            [clojure.lang.named           :refer [name namespace]]
            [clojure.lang.operators       :refer [not not= not== = ==]]
            [clojure.lang.platform.object :refer [identical?]]
            [clojure.lang.show            :refer [str]]
            [clojure.lang.symbol          :refer [symbol]]))

(deftest keyword-test
  (testing "creates a keyword from a name"
    (let [kwd (keyword "kwd")]
      (is (= "kwd" (name kwd)))))

  (testing "has no namespace by default"
    (let [kwd (keyword "kwd")]
      (is (= nil (namespace kwd)))))

  (testing "has a hash code similar to its symbol representation"
    (is (= (hash (keyword "kwd"))
           (+ (hash (symbol "kwd")) 0x9e3779b9)))
    (is (= (hash (keyword "kwd1"))
           (+ (hash (symbol "kwd1")) 0x9e3779b9))))

  (testing "creates a keyword with a namespace"
    (is (= "the-ns" (namespace (keyword "the-ns" "kwd")))))

  (testing "uses the namespace to create the hash"
    (let [kwd (keyword "the-ns" "kwd")
          sym (symbol "the-ns" "kwd")]
    (is (= (hash kwd)
           (hash (+ (hash sym) 0x9e3779b9))))))

  (testing "creates a keyword with a namespace-qualified name"
    (let [kwd (keyword "the-ns/kwd")]
      (is (= "kwd" (name kwd)))
      (is (= "the-ns" (namespace kwd)))))

  (testing "creates a keyword with a namespace-qualified name that has many slashes"
    (let [kwd (keyword "the-ns/kwd/kw2")]
      (is (= "kw2" (name kwd)))
      (is (= "the-ns/kwd" (namespace kwd)))))

  (testing "returns the string representation of the keyword"
    (let [kwd1 (keyword "kwd1")
          kwd2 (keyword "the-ns" "kwd2")]
      (is (= ":kwd1" (str kwd1)))
      (is (= ":the-ns/kwd2" (str kwd2))))))

(deftest keyword?-test
  (testing "returns true if the given object is a keyword"
    (is (= true (keyword? (keyword "kwd")))))

  (testing "returns false if the given object is not a keyword"
    (is (= false (keyword? 1)))
    (is (= false (keyword? nil)))))

(deftest meta-test
  (testing "keywords have no initial metadata"
    (is (= {} (meta (keyword "kwd")))))

  (testing "adds metadata using with-meta"
    (let [kwd1 (keyword "the-ns" "kwd")
          m {:some-meta "here" :private true}
          kwd2 (with-meta kwd1 m)]
      (is (not (identical? kwd1 kwd2)))
      (is (= kwd1 kwd2))

      (is (= m (meta kwd2)))
      (is (= (hash kwd1)
             (hash kwd2)))
      (is (= "the-ns" (namespace kwd2)))
      (is (= "kwd" (name kwd2)))
      (is (= ":the-ns/kwd" (str kwd2))))))

(deftest compare-test
  (testing "equal if ns and name are equal"
    (let [lhs (keyword "the-ns" "kwd")
          rhs (keyword "the-ns" "kwd")]
      (is-equal lhs rhs)))

  (testing "equal if only the name is equal"
    (let [lhs (keyword "kwd")
          rhs (keyword "kwd")]
      (is-equal lhs rhs)))

  (testing "not equal if the names are not equal"
    (is (not= (keyword "kwd1")
              (keyword "kwd2"))))

  (testing "not equal if the names are equal but the namespaces are not"
    (is (not= (keyword "ns1" "kwd")
              (keyword "ns2" "kwd"))))

  (testing "not equal if the namespaces are equal but the names are not"
    (is (not= (keyword "ns" "kwd1")
              (keyword "ns" "kwd2"))))

  (testing "not equal if either is nil"
    (is (not= nil (keyword "kwd")))
    (is (not= (keyword "kwd") nil)))

  (testing "not equal if either is not Named"
    (is (not= 1 (keyword "kwd")))
    (is (not= (keyword "kwd") 1)))

  (testing "loosely equal to symbols"
    (is (= (symbol "kwd") (keyword "kwd")))
    (is (= (keyword "kwd") (symbol "kwd"))))

  (testing "but not strictly equal to symbols"
    (is (not== (symbol "kwd") (keyword "kwd")))
    (is (not== (keyword "kwd") (symbol "kwd"))))

  (testing "loosely equal to vars")

  (testing "returns 0 if the keywords are equal"
    (let [lhs (keyword "kwd")
          rhs (keyword "kwd")]
      (is-equal lhs rhs)))

  (testing "less than if lhs ns is nil and rhs ns is not nil"
    (let [lhs (keyword "kwd")
          rhs (keyword "the-ns" "kwd")]
      (is-less-than lhs rhs)))

  (testing "less than if lhs is nil"
    (let [lhs nil
          rhs (keyword "kwd")]
      (is-less-than lhs rhs)))

  (testing "greater than if lhs ns is not nil and rhs ns is nil"
    (let [lhs (keyword "the-ns1" "kwd")
          rhs (keyword "kwd")]
      (is-greater-than lhs rhs)))

  (testing "greater than if rhs is nil"
    (let [lhs (keyword "kwd")
          rhs nil]
      (is-greater-than lhs rhs)))

  (testing "less than if lhs ns is less than rhs ns"
    (let [lhs (keyword "a" "kwd")
          rhs (keyword "b" "kwd")]
      (is-less-than lhs rhs)))

  (testing "greater than if not equal and if lhs ns is greater than rhs ns"
    (let [lhs (keyword "b" "kwd")
          rhs (keyword "a" "kwd")]
      (is-greater-than lhs rhs)))

  (testing "less than if nses equal and if lhs name is less than rhs name"
    (let [lhs (keyword "ns" "a")
          rhs (keyword "ns" "b")]
      (is-less-than lhs rhs)))

  (testing "greater than if nses equal and if lhs name is greater than rhs name"
    (let [lhs (keyword "ns" "b")
          rhs (keyword "ns" "a")]
      (is-greater-than lhs rhs))))
