(ns clojure.lang.operators-test
  (:refer-clojure :only [bigint double float bigdec defmacro reify let list list* nil?])
  (:require [clojure.test                     :refer :all]
            [clojure.lang.platform.exceptions :refer [class-cast-exception argument-error]]
            [clojure.lang.protocols           :refer [IEquivalence]]
            [clojure.next                     :refer :all]))

(defmacro class-cast-exception-thrown? [& body]
  (list 'is (list* 'thrown? class-cast-exception body)))

(defmacro argument-error-thrown? [& body]
  (list 'is (list* 'thrown? argument-error body)))

(deftest and-test
  (testing "returns true with zero arguments"
    (is (and)))

  (testing "true if the expression is true"
    (is (and true)))

  (testing "returns false if the expressions is false"
    (is (not (and false))))

  (testing "does not evaulate unecessary expressions"
    (is (not (and false (is false))))))

(deftest not-test
  (testing "returns true if falsy"
    (is (= true (not false)))
    (is (= true (not nil))))

  (testing "returns false if truthy"
    (is (= false (not true)))
    (is (= false (not :something)))))

(deftest or-test
  (testing "returns nil with zero arguments"
    (is (nil? (or))))

  (testing "true if the expression is true"
    (is (or true)))

  (testing "false if the expression is false"
    (is (not (or false))))

  (testing "does not evaluate unecessary expressions"
    (is (or true (is false)))))

(deftest =-test
  (testing "calls the -equal? method on the lhs"
    (is (= (reify IEquivalence (-equal? [this other] true))
           :anything))
    (is (not (= (reify IEquivalence (-equal? [this other] false))
                :anything))))

  (testing "not equal if either is nil"
    (is (not= nil :something))
    (is (not= :something nil)))

  (testing "equal if both are nil"
    (is (= nil nil)))

  (testing "true if only one item is given"
    (is (= :one-item)))

  (testing "more than two items -  true if every item is equal to each other"
    (is (= 1 1 1))
    (is (not (= 1 1 2)))
    (is (= 1 1 1 1))
    (is (not (= 1 1 1 2))))

  )

(deftest ==-test
  (testing "calls the -equivalent? method on the lhs"
    (is (== (reify IEquivalence (-equivalent? [this other] true))
            :anything))
    (is (not (== (reify IEquivalence (-equivalent? [this other] false))
                 :anything))))

  (testing "not equal if either is nil"
    (is (not== nil :something))
    (is (not== :something nil)))

  (testing "equal if both are nil"
    (is (== nil nil)))

  (testing "true if only one item is given"
    (is (== :something)))

  (testing "more than two items -  true if every item is equivalent to each other"
    (let [item1 (reify IEquivalence (-equivalent? [this other] true))
          item2 (reify IEquivalence (-equivalent? [this other] true))
          item3 (reify IEquivalence (-equivalent? [this other] true))
          item4 (reify IEquivalence (-equivalent? [this other] false))
          item5 (reify IEquivalence (-equivalent? [this other] false))
          ]
      (is (== item1 item2 item3))
      (is (== item1 item2 item4))
      (is (not== item1 item4 item2))
      (is (not== item1 item4 item5))))

  )

(deftest bit-and-test
  (testing "returns for two arguments"
    (is (= 1 (bit-and 1 1))))

  (testing "returns for many arguments"
    (is (= 1 (bit-and 1 1 1))))

  (testing "raises an error with big numbers and decimals"
    (argument-error-thrown? (bit-and (bigint 1) 1))
    (argument-error-thrown? (bit-and (double 0.1) 1))
    (argument-error-thrown? (bit-and (float 0.1) 1))
    (argument-error-thrown? (bit-and (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-thrown? (bit-and "foo" 1)))

  )

(deftest +-test
  (testing "returns 0 without arguments"
    (is (= 0 (+))))

  (testing "acts as an identity function with 1 argument"
    (is (= 42 (+ 42))))

  (testing "adds two numbers together"
    (is (= 42 (+ 22 20))))

  (testing "adds many numbers together"
    (is (= 42 (+ 1 1 40))))

  (testing "raises an error without numbers"
    (class-cast-exception-thrown? (+ "Foo"))
    (class-cast-exception-thrown? (+ 1 "Foo")))

  )

(deftest inc-test
  (testing "increment an argument"
    (is (= 2 (inc 1))))

  (testing "raises an error without numbers"
    (class-cast-exception-thrown? (inc "Foo")))

  )

(deftest *-test
  (testing "returns 1 without arguments"
    (is (= 1 (*))))

  (testing "acts as an identity function with 1 argument"
    (is (= 42 (* 42))))

  (testing "multiplies two numbers together"
    (is (= 42 (* 21 2))))

  (testing "multiplies many numbers together"
    (is (= 42 (* 1 2 21))))

  (testing "raises an error without numbers"
    (class-cast-exception-thrown? (* "Foo"))
    (class-cast-exception-thrown? (* 1 "Foo")))

  )

(deftest --test
  (testing "negation of a single element"
    (is (= -1 (- 1))))

  (testing "subtracting two numbers"
    (is (= 1 (- 3 2))))

  (testing "subtracts many numbers"
    (is (= 1 (- 5 3 1))))

  (testing "raises an error without numbers"
    (class-cast-exception-thrown? (- "Foo"))
    (class-cast-exception-thrown? (- 1 "Foo")))

  )

(deftest dec-test
  (testing "decrement an argument"
    (is (= 1 (dec 2))))

  (testing "raises an error without numbers"
    (class-cast-exception-thrown? (dec "Foo")))

  )

(deftest divide-test ; / is not a valid character
  (testing "ratio creation with a single argument"
    (let [ratio (/ 2)]
      (is (= 1 (numerator ratio)))
      (is (= 2 (denominator ratio)))))

  (testing "divides two numbers"
    (is (= 2 (/ 4 2))))

  (testing "divides many numbers"
    (is 2 (/ 12 2 3)))

  (testing "raises an error without numbers"
    (class-cast-exception-thrown? (/ "Foo"))
    (class-cast-exception-thrown? (/ 1 "Foo")))

  )
