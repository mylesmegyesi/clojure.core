(ns clojure.lang.operators-test
  (:refer-clojure :only [defprotocol deftype extend-protocol fn reify let nil?])
  (:require [clojure.test           :refer :all]
            [clojure.lang.protocols :refer [IEquivalence MathOperations]]
            [clojure.next           :refer :all]))

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

(defprotocol MathResultValue
  (-unbox-math-result [this]))

(extend-protocol MathResultValue
  Object
  (-unbox-math-result [this] this))

(deftype AddResult [lhs rhs]
  MathOperations
  (-add [x y] (AddResult. x y))

  MathResultValue
  (-unbox-math-result [this] [(-unbox-math-result lhs)
                              (-unbox-math-result rhs)]))

(deftest +-test
  (testing "returns 0 if there are no arguments"
    (is (= 0 (+))))

  (testing "just returns the argument if there are is only one argument"
    (is (= :x (+ :x))))

  (testing "calls the -add method on the lhs when two arguments"
    (let [just-ret-args (reify MathOperations (-add [x y] [x y]))]
      (is (= (+ just-ret-args 6)
             [just-ret-args 6]))))

  (testing "sums all args together"
    (let [adder (reify MathOperations (-add [x y] (AddResult. x y)))
          res   (+ adder 6 7 8)]
      (is (= (-unbox-math-result res)
             [[[adder 6] 7] 8]))))

  )

(deftype SubtractResult [lhs rhs]
  MathOperations
  (-subtract [x y] (SubtractResult. x y))

  MathResultValue
  (-unbox-math-result [this] [(-unbox-math-result lhs)
                              (-unbox-math-result rhs)]))

(deftest --test
  (testing "calls -subtract on the argument if there are is only one argument"
    (let [just-ret-arg (reify MathOperations (-subtract [x] x))]
      (is (= just-ret-arg (- just-ret-arg)))))

  (testing "calls the -subtract method on the lhs when two arguments"
    (let [just-ret-args (reify MathOperations (-subtract [x y] [x y]))]
      (is (= (- just-ret-args 6)
             [just-ret-args 6]))))

  (testing "subtracts all args together"
    (let [subtracter (reify MathOperations (-subtract [x y] (SubtractResult. x y)))
          res (- subtracter 6 7 8)]
      (is (= (-unbox-math-result res)
             [[[subtracter 6] 7] 8]))))

  )

(deftype MultResult [lhs rhs]
  MathOperations
  (-multiply [x y] (MultResult. x y))

  MathResultValue
  (-unbox-math-result [this] [(-unbox-math-result lhs)
                              (-unbox-math-result rhs)]))

(deftest +-test
  (testing "returns 1 with no args"
    (is (= 1 (*))))

  (testing "returns the argument when one argument is given"
    (is (= :x (* :x))))

  (testing "calls -multiply on the argument when two args given"
    (let [just-ret-args (reify MathOperations (-multiply [x y] [x y]))]
      (is (= (* just-ret-args 6)
             [just-ret-args 6]))))

  (testing "multiplies all args together"
    (let [multiplier (reify MathOperations (-multiply [x y] (MultResult. x y)))
          res (* multiplier 6 7 8)]
      (is (= (-unbox-math-result res)
             [[[multiplier 6] 7] 8]))))

  )
