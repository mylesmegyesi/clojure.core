(ns clojure.lang.persistent-list-test
  (:refer-clojure :only [defmacro let list* apply range])
  (:require [clojure.test                 :refer :all]
            [clojure.lang.persistent-list :refer [EMPTY-LIST list]]
            [clojure.lang.exceptions      :refer [illegal-state-error]]
            [clojure.next                 :refer :all]))

(defmacro illegal-state-error-is-thrown? [& body]
  (clojure.core/list 'is (list* 'thrown? illegal-state-error body)))

(deftest count-test
  (testing "returns 0 for empty list"
    (is (= 0 (count (list)))))

  (testing "returns 0 the count"
    (is (= 1 (count (list :a))))
    (is (= 2 (count (list :a :b))))
    (is (= 3 (count (list :a :b :c)))))
)

(deftest first-test
  (testing "returns nil for empty"
    (is (= nil (first (list)))))

  (testing "returns first element"
    (is (= "A" (first (list "A" "B" "C")))))
)

(deftest next-test
  (testing "returns nil for one element"
    (is (= nil (next (list 1)))))

  (testing "returns next"
    (is (= (list 2 3) (next (list 1 2 3)))))
)

(deftest rest-test
  (testing "returns an empty list for one element"
    (is (= EMPTY-LIST (rest (list 1)))))

  (testing "returns rest"
    (is (= (list 2 3) (rest (list 1 2 3)))))
)

(deftest list?-test
  (testing "is a list"
    (is (list? (list))))
)

(deftest meta-test
  (testing "meta"
    (is (= nil (meta (list :meta-list)))))

  (testing "with-meta"
    (let [meta-list (with-meta (list "a" "b") {:my :meta})]
      (is (= :meta (:my (meta meta-list))))))
)

(deftest empty-test
  (testing "returns empty list"
    (is (= 0 (count (empty (list 1 2 3))))))

  (testing "preserves meta"
    (let [new-list (with-meta (list :a :b :c) {:this :that})
          empty-list (empty new-list)]
      (is (= 0 (count empty-list)))
      (is (= :that (:this (meta empty-list))))))
)

(deftest peek-test
  (testing "returns first element"
    (let [new-list (list 1 2 3 4)]
      (is (= 1 (peek new-list)))
      (is (= 2 (peek (next new-list))))))
)

(deftest pop-test
  (testing "returns the rest"
    (let [new-list (list "one" "two" "three")]
      (is (= (list "two" "three") (pop new-list)))))

  (testing "throws exception for empty list"
    (illegal-state-error-is-thrown?
      (pop (list))))
)

(deftest seq-test
  (testing "return nil for empty list"
    (is (= nil (seq (list)))))
)

