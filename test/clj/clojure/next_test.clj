(ns clojure.next-test
  (:refer-clojure :only [*assert* apply binding defmacro deftype eval let reify subs var-set])
  (:require [clojure.test                         :refer :all]
            [clojure.next                         :refer :all]
            [clojure.lang.exceptions              :refer [new-exception]]
            [clojure.lang.protocols               :refer :all]
            [clojure.support.exception-assertions :refer [argument-error-is-thrown?
                                                          assertion-error-is-thrown?]]
            [clojure.support.test-seq             :refer [test-seq]]))

(deftest clojure-version-test
  (testing "current clojure version as a string"
    (is (= "1.6.0" (clojure-version)))))

(deftest comment-test
  (testing "comment returns nil"
    (is (nil? (comment))))

  (testing "the body is not evaluated"
    (is (nil? (comment (throw (new-exception)))))))

(deftest type-test
  (testing "will use the :type meta value"
    (let [foo-typed (with-meta (vector) (array-map :type "foo"))]
      (is (= "foo" (type foo-typed))))))

(deftest constantly-test
  (testing "returns the return value"
    (is (= :val ((constantly :val)))))

  (testing "takes variable args and still returns the return value"
    (let [constant-fn (constantly :val)]
      (clojure.core/map #(is (= :val (apply constant-fn %)))
        [[] [1] [1 2] [1 2 3]]))))

(deftest when-not-test
  (testing "returns nil if the test returns true"
    (is (nil? (when-not (= 1 1) :bar))))

  (testing "return the value if the test returns not true"
    (is (= :bar (when-not (= 1 2) :bar)))))

(deftest associative?-test
  (testing "returns true when associative"
    (is (associative? (reify IAssociative))))

  (testing "returns false otherwise"
    (is (not (associative? "foo")))))

(deftest identity-test
  (testing "identity returns the provided argument"
    (is (= :foo (identity :foo)))))

(deftest assert-test
  (testing "*assert* is true by default"
    (is (true? *assert*)))

  (testing "assert raises an assertion error if the test fails"
    (assertion-error-is-thrown? #"Assert failed" (assert (= 1 2))))

  (testing "assert does not raise an assertion error if the test fails"
    (is (nil? (assert (= 1 1)))))

  (testing "assert raises an assertion error with a message"
    (assertion-error-is-thrown? #"Assert failed: test" (assert (= 1 2) "test")))

  (testing "assert does not run if *assert* is bound to false"
    (is
      (nil? (binding [*assert* false] (eval '(assert false)))))))

(deftest get-test
  (testing "returns nil when the object does not satisfy ILookup"
    (is (nil? (get :foo :bar)))))

(deftest conj-test
  (testing "conj with no arguments returns an empty vector"
    (is (= [] (conj))))

  (testing "conj with one argument returns the argument"
    (is (= '(1 2 3) (conj '(1 2 3)))))

  (testing "conj two arguments together"
    (let [conj-arg (atom nil)
          conjable (reify
                     IPersistentCollection
                     (-cons [this x]
                       (reset! conj-arg x)
                       this))]
      (conj conjable :val)
      (is (= :val (deref conj-arg)))))

  (testing "conj many arguments together"
    (let [conj-args (atom nil)
          conjable (reify
                     IPersistentCollection
                     (-cons [this x]
                       (reset! conj-args (cons x (deref conj-args)))
                       this))]
      (conj conjable :val1 :val2 :val3)
      (is (= :val3 (first (deref conj-args))))
      (is (= :val2 (first (next (deref conj-args)))))
      (is (= :val1 (first (next (next (deref conj-args)))))))))

(deftest set?-test
  (testing "returns true for an IPersistentSet"
    (let [s (reify IPersistentSet)]
      (is (set? s))))

  (testing "returns false for not a set"
    (is (not (set? 1)))))

(deftest coll?-test
  (testing "returns true for a IPersistentCollection"
    (let [c (reify IPersistentCollection)]
      (is (coll? c))))

  (testing "returns false for not a coll"
    (is (not (coll? 1)))))

(deftest list?-test
  (testing "returns true for a IPersistentList"
    (let [l (reify IPersistentList)]
      (is (list? l))))

  (testing "returns false for not a list"
    (is (not (list? 1)))))

(deftest string?-test
  (testing "returns true for a String"
    (is (string? "foo")))

  (testing "returns false otherwise"
    (is (not (string? :foo)))))

(deftest counted?-test
  (testing "returns true for a ICounted"
    (let [c (reify ICounted)]
      (is (counted? c))))

  (testing "returns false otherwise"
    (is (not (counted? :anything)))))

(deftest contains?-test
  (testing "returns false for nil"
    (is (false? (contains? nil :anything))))

  (testing "throws an argument-error for an unhandlable type"
    (argument-error-is-thrown? #"contains\? not supported on type" (contains? :anything :anything))))

(deftest count-test
  (testing "count a persistent collection that does not implement ICounted"
    (let [coll (reify
                 IPersistentCollection
                 ISeqable
                 (-seq [this] (test-seq '(1 2 3))))]
      (is (= 3 (count coll))))))

(deftest while-test
  (testing "while completes if the test is falsy"
    (is (nil? (while false))))

  (testing "while runs while a test is truthy"
    (let [atm (atom true)
          fut (future (while (deref atm)))]
      (is (not (future-done? fut)))
      (reset! atm false)
      (deref fut)
      (is (future-done? fut)))))

(deftest into-test
  (testing "put all of one collection into another"
    (let [to (array-map 1 2)
          from (hash-map 3 4)]
      (is (= (array-map 1 2 3 4) (into to from)))))

  (testing "IEditableCollections retain their meta"
    (let [coll (vector 1 2 3)
          meta-coll (with-meta coll {:so :meta})
          res (into meta-coll (vector 4 5 6))]
      (is (= {:so :meta} (meta res))))))

(deftest time-test
  (testing "displaying time elapsed with evaluating an expression in milliseconds"
    (let [out-str (with-out-str (time (+ 1 1)))]
      (is (= (subs out-str 1 14) "Elapsed time:"))))

  (testing "time returns the result of the expression"
    (with-out-str
      (is (= 2 (time (+ 1 1)))))))

