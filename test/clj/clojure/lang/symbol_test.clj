(ns clojure.lang.symbol-test
  (:refer-clojure :only [let get nil? defn])
  (:require [clojure.test                 :refer :all]
            [clojure.lang.assertions      :refer :all]
            [clojure.lang.comparison      :refer [= not= ==]]
            [clojure.lang.hash            :refer [hash]]
            [clojure.lang.keyword         :refer [keyword]]
            [clojure.lang.logical         :refer [not]]
            [clojure.lang.meta            :refer [meta with-meta]]
            [clojure.lang.named           :refer [name namespace]]
            [clojure.lang.platform.object :refer [identical?]]
            [clojure.lang.show            :refer [str]]
            [clojure.lang.symbol          :refer :all]))

(deftest symbol-test
  (testing "creates a symbol given just a name"
    (is (= "sym-name" (name (symbol "sym-name")))))

  (testing "creates a symbol given a name and a namespace"
    (is (= "sym-name" (name (symbol "the-ns" "sym-name")))))

  (testing "creates a symbol given a namespace-qualified name"
    (let [sym (symbol "the-ns/sym-name")]
      (is (= "sym-name" (name sym)))
      (is (= "the-ns" (namespace sym)))))

  (testing "creates a symbol given a namespace-qualified name and namespace"
    (let [sym (symbol "other-ns" "the-ns/sym-name")]
      (is (= "the-ns/sym-name" (name sym)))
      (is (= "other-ns" (namespace sym)))))

  (testing "creates a symbol given a namespace-qualified name that has many slashes"
    (let [sym (symbol "the-ns/sym-name/other-name")]
      (is (= "other-name" (name sym)))
      (is (= "the-ns/sym-name" (namespace sym)))))

  (testing "just returns the symbol if given a symbol"
    (let [sym (symbol "sym")]
      (is (identical? sym (symbol sym)))))

  (testing "returns the namespace of the symbol"
    (is (= "the-ns" (namespace (symbol "the-ns" "sym-name")))))

  (testing "can be used a key in a map"
    (let [sym (symbol "sym")]
      (is (= 1 (get {sym 1} sym)))))

  (testing "two different symbols are the same in a map"
    (let [sym1 (symbol "sym")
          sym2 (symbol "sym")]
      (is (not (identical? sym1 sym2)))
      (is (= 1 (get {sym1 1} sym1)))
      (is (= 1 (get {sym1 1} sym2)))
      (is (= 1 (get {sym2 1} sym1)))
      (is (= 1 (get {sym2 1} sym2)))))

  (testing "cannot create a symbol with nil name"
    (is (thrown-with-msg? Exception #"Can't create symbol with nil name"
          (symbol nil nil)))))

(deftest symbol?-test
  (testing "returns true if the given object is a symbol"
    (is (= true
           (symbol? (symbol "sym")))))

  (testing "returns false if the given object is a not symbol"
    (is (= false (symbol? 1)))))

(deftest meta-test
  (testing "symbols have no initial metadata"
    (is (= {} (meta (symbol "sym")))))

  (testing "adds metadata using with-meta"
    (let [sym1 (symbol "the-ns" "sym")
          m {:some-meta "here" :private true}
          sym2 (with-meta sym1 m)]
      (is (not (identical? sym1 sym2)))

      (is (= m (meta sym2)))
      (is (= (hash sym1)
             (hash sym2)))
      (is (= "the-ns" (namespace sym2)))
      (is (= "sym" (name sym2)))
      (is (= "the-ns/sym" (str sym2))))))

(deftest compare-test
  (testing "equal if ns and name are equal"
    (let [lhs (symbol "the-ns" "sym")
          rhs (symbol "the-ns" "sym")]
      (is-equal lhs rhs)))

  (testing "equal if only the name is equal"
    (let [lhs (symbol "sym")
          rhs (symbol "sym")]
      (is-equal lhs rhs)))

  (testing "not equal if the names are not equal"
    (is (not= (symbol "sym1")
              (symbol "sym2"))))

  (testing "not equal if the names are equal but the namespaces are not"
    (is (not= (symbol "ns1" "sym")
              (symbol "ns2" "sym"))))

  (testing "not equal if the namespaces are equal but the names are not"
    (is (not= (symbol "ns" "sym1")
              (symbol "ns" "sym2"))))

  (testing "not equal if either is nil"
    (is (not= nil (symbol "sym")))
    (is (not= (symbol "sym") nil)))

  (testing "not equal if either does not implement the Named protocol"
    (is (not= 1 (symbol "sym")))
    (is (not= (symbol "sym") 1)))

  (testing "can be equal to other Named types"
    (is (= (symbol "kwd") (keyword "kwd")))
    (is (= (keyword "kwd") (symbol "kwd"))))

  (testing "returns 0 if the symbols are equal"
    (let [lhs (symbol "sym")
          rhs (symbol "sym")]
      (is-equal lhs rhs)))

  (testing "less than if lhs ns is nil and rhs ns is not nil"
    (let [lhs (symbol "sym")
          rhs (symbol "the-ns" "sym")]
      (is-less-than lhs rhs)))

  (testing "less than if lhs is nil"
    (let [lhs nil
          rhs (symbol "sym")]
      (is-less-than lhs rhs)))

  (testing "greater than if lhs ns is not nil and rhs ns is nil"
    (let [lhs (symbol "the-ns1" "sym")
          rhs (symbol "sym")]
      (is-greater-than lhs rhs)))

  (testing "greater than if rhs is nil"
    (let [lhs (symbol "sym")
          rhs nil]
      (is-greater-than lhs rhs)))

  (testing "less than if lhs ns is less than rhs ns"
    (let [lhs (symbol "a" "sym")
          rhs (symbol "b" "sym")]
      (is-less-than lhs rhs)))

  (testing "greater than if not equal and if lhs ns is greater than rhs ns"
    (let [lhs (symbol "b" "sym")
          rhs (symbol "a" "sym")]
      (is-greater-than lhs rhs)))

  (testing "less than if nses equal and if lhs name is less than rhs name"
    (let [lhs (symbol "ns" "a")
          rhs (symbol "ns" "b")]
      (is-less-than lhs rhs)))

  (testing "greater than if nses equal and if lhs name is greater than rhs name"
    (let [lhs (symbol "ns" "b")
          rhs (symbol "ns" "a")]
      (is-greater-than lhs rhs))))
