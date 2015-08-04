(ns clojure.lang.operators-test
  (:refer-clojure :only [apply concat defmacro defn doseq reify let list list* remove resolve])
  (:require [clojure.test                         :refer :all]
            [clojure.lang.protocols               :refer [IEquivalence]]
            [clojure.support.exception-assertions :refer [argument-error-is-thrown?
                                                          arithmetic-exception-is-thrown?
                                                          class-cast-exception-is-thrown?]]
            [clojure.next                         :refer :all]))

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
  (testing  "raises an error when arguments are not numbers"
    (class-cast-exception-is-thrown? #"" (== :a :a)))

  (testing "true if only one item is given"
    (is (== :something)))

  (testing "true if both are equal"
    (is (== 1000 1000))
    (is (not== 1000 1001)))

  (testing "more than two items - true if every item is equivalent to each other"
    (is (== 5 5 5))
    (is (== 10 10 10))
    (is (not== 20 21 20))
    (is (not== 30 30 31)))

  )

(deftest byte-test
  (testing "raises an error when cast from less than min byte"
    (is (= -128 (byte -128)))
    (argument-error-is-thrown? #"" (byte -129)))

  (testing "raises an error when cast from greater than max byte"
    (is (= 127 (byte 127)))
    (argument-error-is-thrown? #"" (byte 128)))

  )

(deftest short-test
  (testing "raises an error when cast from less than min short"
    (is (= -32768 (short -32768)))
    (argument-error-is-thrown? #"" (short -32769)))

  (testing "raises an error when cast from greater than max short"
    (is (= 32767 (short 32767)))
    (argument-error-is-thrown? #"" (short 32768)))

  )

(deftest int-test
  (testing "raises an error when cast from less than min int"
    (is (= -2147483648 (int -2147483648)))
    (argument-error-is-thrown? #"" (int -2147483649)))

  (testing "raises an error when cast from greater than max int"
    (is (= 2147483647 (int 2147483647)))
    (argument-error-is-thrown? #"" (int 2147483648)))

  )

(deftest long-test
  (testing "raises an error when cast from less than min long"
    (is (= -9223372036854775808 (long -9223372036854775808)))
    (argument-error-is-thrown? #"" (long -9223372036854775809)))

  (testing "raises an error when cast from greater than max long"
    (is (= 9223372036854775807 (long 9223372036854775807)))
    (argument-error-is-thrown? #"" (long 9223372036854775808)))

  )

(deftest bit-shift-right-test
  (testing "returns for a numbers and a shift"
    (is (= 1 (bit-shift-right 3 1))))

 (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-shift-right (bigint 1) 1))
    (argument-error-is-thrown? #"" (bit-shift-right (double 0.1) 1))
    (argument-error-is-thrown? #"" (bit-shift-right (float 0.1) 1))
    (argument-error-is-thrown? #"" (bit-shift-right (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-shift-right "foo" 1)))

  )

(deftest bit-unsigned-shift-right-test
  (testing "returns for a numbers and a shift"
    (is (= 1 (unsigned-bit-shift-right 3 1))))

 (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (unsigned-bit-shift-right (bigint 1) 1))
    (argument-error-is-thrown? #"" (unsigned-bit-shift-right (double 0.1) 1))
    (argument-error-is-thrown? #"" (unsigned-bit-shift-right (float 0.1) 1))
    (argument-error-is-thrown? #"" (unsigned-bit-shift-right (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (unsigned-bit-shift-right "foo" 1)))

  )

(deftest bit-not-test
  (testing "returns for a number"
    (is (= 41 (bit-not -42))))

  (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-not (bigint 1)))
    (argument-error-is-thrown? #"" (bit-not (double 0.1)))
    (argument-error-is-thrown? #"" (bit-not (float 0.1)))
    (argument-error-is-thrown? #"" (bit-not (bigdec 0.1))))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-not "foo")))

  )

(deftest bit-and-test
  (testing "returns for two arguments"
    (is (= 1 (bit-and 1 1))))

  (testing "returns for many arguments"
    (is (= 1 (bit-and 1 1 1))))

  (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-and (bigint 1) 1))
    (argument-error-is-thrown? #"" (bit-and (double 0.1) 1))
    (argument-error-is-thrown? #"" (bit-and (float 0.1) 1))
    (argument-error-is-thrown? #"" (bit-and (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-and "foo" 1)))

  )

(deftest bit-and-not-test
  (testing "returns for two arguments"
    (is (= 0 (bit-and-not 1 1))))

  (testing "returns for many arguments"
    (is (= 0 (bit-and-not 1 1 1))))

  (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-and-not (bigint 1) 1))
    (argument-error-is-thrown? #"" (bit-and-not (double 0.1) 1))
    (argument-error-is-thrown? #"" (bit-and-not (float 0.1) 1))
    (argument-error-is-thrown? #"" (bit-and-not (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-and-not "foo" 1)))

  )

(deftest bit-or-test
  (testing "returns for two arguments"
    (is (= 1 (bit-or 1 1))))

  (testing "returns for many arguments"
    (is (= 1 (bit-or 1 1 1))))

 (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-or (bigint 1) 1))
    (argument-error-is-thrown? #"" (bit-or (double 0.1) 1))
    (argument-error-is-thrown? #"" (bit-or (float 0.1) 1))
    (argument-error-is-thrown? #"" (bit-or (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-or "foo" 1)))

  )

(deftest bit-xor-test
  (testing "returns for two arguemnts"
    (is (= 0 (bit-xor 1 1))))

  (testing "returns for many arguments"
    (is (= 1 (bit-xor 1 1 1))))

 (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-xor (bigint 1) 1))
    (argument-error-is-thrown? #"" (bit-xor (double 0.1) 1))
    (argument-error-is-thrown? #"" (bit-xor (float 0.1) 1))
    (argument-error-is-thrown? #"" (bit-xor (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-xor "foo" 1)))

  )

(deftest bit-clear-test
  (testing "returns for a number and a position"
    (is (= 0 (bit-clear 1 0))))

 (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-clear (bigint 1) 1))
    (argument-error-is-thrown? #"" (bit-clear (double 0.1) 1))
    (argument-error-is-thrown? #"" (bit-clear (float 0.1) 1))
    (argument-error-is-thrown? #"" (bit-clear (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-clear "foo" 1)))

  )

(deftest bit-set-test
  (testing "returns for a number and a position"
    (is (= 1 (bit-set 1 0))))

 (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-set (bigint 1) 1))
    (argument-error-is-thrown? #"" (bit-set (double 0.1) 1))
    (argument-error-is-thrown? #"" (bit-set (float 0.1) 1))
    (argument-error-is-thrown? #"" (bit-set (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-set "foo" 1)))

  )

(deftest bit-flip-test
  (testing "returns for a number and a position"
    (is (= 5 (bit-flip 1 2))))

 (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-flip (bigint 1) 1))
    (argument-error-is-thrown? #"" (bit-flip (double 0.1) 1))
    (argument-error-is-thrown? #"" (bit-flip (float 0.1) 1))
    (argument-error-is-thrown? #"" (bit-flip (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-flip "foo" 1)))

  )

(deftest bit-test-test
  (testing "returns for a number and a position"
    (is (= false (bit-test 1 2))))

 (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-test (bigint 1) 1))
    (argument-error-is-thrown? #"" (bit-test (double 0.1) 1))
    (argument-error-is-thrown? #"" (bit-test (float 0.1) 1))
    (argument-error-is-thrown? #"" (bit-test (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-test "foo" 1)))

  )

(deftest bit-shift-left-test
  (testing "returns for a numbers and a shift"
    (is (= 2 (bit-shift-left 1 1))))

 (testing "raises an error with big numbers and decimals"
    (argument-error-is-thrown? #"" (bit-shift-left (bigint 1) 1))
    (argument-error-is-thrown? #"" (bit-shift-left (double 0.1) 1))
    (argument-error-is-thrown? #"" (bit-shift-left (float 0.1) 1))
    (argument-error-is-thrown? #"" (bit-shift-left (bigdec 0.1) 1)))

  (testing "raises an error without a number type"
    (argument-error-is-thrown? #"" (bit-shift-left "foo" 1)))

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
    (class-cast-exception-is-thrown? #"" (+ "Foo"))
    (class-cast-exception-is-thrown? #"" (+ 1 "Foo")))

  )

(deftest +'-test
  (testing "returns 0 without arguments"
    (is (= 0 (+'))))

  (testing "acts as an identity function with 1 argument"
    (is (= 42 (+' 42))))

  (testing "adds two numbers together"
    (is (= 42 (+' 22 20))))

  (testing "adds many numbers together"
    (is (= 42 (+' 1 1 40))))

  (testing "raises an error without numbers"
    (class-cast-exception-is-thrown? #"" (+' "Foo"))
    (class-cast-exception-is-thrown? #"" (+' 1 "Foo")))

  )

(deftest inc-test
  (testing "increment an argument"
    (is (= 2 (inc 1))))

  (testing "raises an error without numbers"
    (class-cast-exception-is-thrown? #"" (inc "Foo")))

  )

(deftest inc'-test
  (testing "increment an argument"
    (is (= 2 (inc 1))))

  (testing "raises an error without numbers"
    (class-cast-exception-is-thrown? #"" (inc "Foo")))

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
    (class-cast-exception-is-thrown? #"" (* "Foo"))
    (class-cast-exception-is-thrown? #"" (* 1 "Foo")))

  )

(deftest *'-test
  (testing "returns 1 without arguments"
    (is (= 1 (*'))))

  (testing "acts as an identity function with 1 argument"
    (is (= 42 (*' 42))))

  (testing "multiplies two numbers together"
    (is (= 42 (*' 21 2))))

  (testing "multiplies many numbers together"
    (is (= 42 (*' 1 2 21))))

  (testing "raises an error without numbers"
    (class-cast-exception-is-thrown? #"" (*' "Foo"))
    (class-cast-exception-is-thrown? #"" (*' 1 "Foo")))

  )

(deftest --test
  (testing "negation of a single element"
    (is (= -1 (- 1))))

  (testing "subtracting two numbers"
    (is (= 1 (- 3 2))))

  (testing "subtracting many numbers"
    (is (= 1 (- 5 3 1))))

  (testing "raises an error without numbers"
    (class-cast-exception-is-thrown? #"" (- "Foo"))
    (class-cast-exception-is-thrown? #"" (- 1 "Foo")))

  )

(deftest -'-test
  (testing "negation of a single element"
    (is (= -1 (-' 1))))

  (testing "subtracting two numbers"
    (is (= 1 (-' 3 2))))

  (testing "subtracting many numbers"
    (is (= 1 (-' 5 3 1))))

  (testing "raises an error without numbers"
    (class-cast-exception-is-thrown? #"" (-' "Foo"))
    (class-cast-exception-is-thrown? #"" (-' 1 "Foo")))

  )

(deftest dec-test
  (testing "decrement an argument"
    (is (= 1 (dec 2))))

  (testing "raises an error without numbers"
    (class-cast-exception-is-thrown? #"" (dec "Foo")))

  )

(deftest dec'-test
  (testing "decrement an argument"
    (is (= 1 (dec 2))))

  (testing "raises an error without numbers"
    (class-cast-exception-is-thrown? #"" (dec "Foo")))

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
    (class-cast-exception-is-thrown? #"" (/ "Foo"))
    (class-cast-exception-is-thrown? #"" (/ 1 "Foo")))

  )

(def int-types [byte short int long bigint biginteger])
(def float-types [double float bigdec])
(def types (concat int-types float-types))

(deftest quot-test
  (doseq [x int-types
          y int-types]
    (testing (str x " quot with " y)
      (is (= 1 (quot (x 1) (y 1))))))

  (doseq [x float-types
          y float-types]
    (testing (str x " quot with " y)
      (is (or (= 1.0 (quot (x 1) (y 1)))
              (= 1M (quot (x 1) (y 1)))))))

  (doseq [x int-types
          y float-types]
    (testing (str x " quot with " y)
      (is (or (= 1.0 (quot (x 1) (y 1)))
              (= 1M (quot (x 1) (y 1)))))))

  (testing "quot throws an exception if dividing by 0"
    (doseq [x types
            y types]
      (arithmetic-exception-is-thrown? #"Divide by zero" (quot (x 1) (y 0))))))

(deftest rem-test
  (doseq [x int-types
          y int-types]
    (testing (str x " rem with " y)
      (is (= 1 (rem (x 3) (y 2))))))

  (doseq [x float-types
          y float-types]
    (testing (str x " rem with " y)
      (is (or (= 1.0 (rem (x 3) (y 2)))
              (= 1M (rem (x 3) (y 2)))))))

  (doseq [x int-types
          y float-types]
    (testing (str x " rem with " y)
      (is (or (= 1.0 (rem (x 3) (y 2)))
              (= 1M (rem (x 3) (y 2)))))))

  (testing "rem throws an exception if dividing by 0"
    (doseq [x types
            y types]
      (arithmetic-exception-is-thrown? #"Divide by zero" (rem (x 1) (y 0))))))

(deftest mod-test
  (testing "modulus is returned as zero if zero"
    (is (zero? (mod 1 1))))

  (testing "modulus is returned if both arguments are positive"
    (is (= 1 (mod 3 2))))

  (testing "modulus is returned if both arguments are negative"
    (is (= -1 (mod -3 -2))))

  (testing "modulus is added to the second arguments if the signs do not match"
    (is (= -1 (mod 3 -2)))))

(deftest zero-test
  (doseq [x types]
    (testing (str x " with a zero value is zero?")
      (is (zero? (x 0)))))

  (testing "raises an error without a number argument"
    (class-cast-exception-is-thrown? #"" (zero? "Foo"))))

(deftest <-test
  (testing "< with one argument returns true"
    (is (< nil)))

  (doseq [x types
          y types]
    (testing (str x " is able to be less than " y)
      (is (< (x 1) (y 2))))

    (testing (str x " is able to be not less than " y)
      (is (not (< (x 2) (y 1)))))

    (testing (str x " is not less than " y " when they are equal")
      (is (not (< (x 1) (y 1))))))

  (testing "many numbers are less than eachother"
    (is (< 1 2 3 4 5)))

  (testing "many numbers are not less than eachother"
    (is (not (< 1 2 5 4 3)))))

(deftest >-test
  (testing "> with one argument returns true"
    (is (> nil)))

  (doseq [x types
          y types]
    (testing (str x " is able to be greater than " y)
      (is (> (x 2) (y 1))))

    (testing (str x " is able to be not greater than " y)
      (is (not (> (x 1) (y 2)))))

    (testing (str x " is not greater than " y " when they are equal")
      (is (not (> (x 1) (y 1))))))

  (testing "many numbers are greater than eachother"
    (is (> 5 4 3 2 1)))

  (testing "many numbers not greater than eachother"
    (is (not (> 5 4 1 2 3)))))

(deftest <=-test
  (testing "<= with one argument returns true"
    (is (<= nil)))

  (doseq [x types
          y types]
    (testing (str x " is able to be less than " y)
      (is (<= (x 1) (y 2))))

    (testing (str x " is able to be not less than " y)
      (is (not (<= (x 2) (y 1)))))

    (testing (str x " can be equal to " y)
      (is (<= (x 1) (y 1)))))

  (testing "many numbers are less than or equal to eachother"
    (is (<= 1 2 3 3 4 5)))

  (testing "many numbers are not less than or equal to eachother"
    (is (not (<= 1 2 3 3 5 4)))))

(deftest >=-test
  (testing ">= with one argument returns true"
    (is (>= nil)))

  (doseq [x types
          y types]
    (testing (str x " is able to be greater than " y)
      (is (>= (x 2) (y 1))))

    (testing (str x " is able to be not greater than " y)
      (is (not (>= (x 1) (y 2)))))

    (testing (str x " can be equal to " y)
      (is (>= (x 1) (y 1)))))

  (testing "many numbers are greater than or equal to eachother"
    (is (>= 5 4 3 3 2 1)))

  (testing "many numbers are not greater than or equal to eachother"
    (is (not (>= 5 4 3 3 1 2)))))

(defn conversion-test [test conversion]
  (let [test-without-question-mark (apply str (remove #(= \? %) (str test)))]
    (testing (str test " converts an integer to a type of " test-without-question-mark)
      (is (@(resolve test) (@(resolve conversion) 42))))

    (testing (str test " converts a float to a type of " test-without-question-mark)
      (is (@(resolve test) (@(resolve conversion) 42.2))))

    (testing (str test " converts a big int to a type of " test-without-question-mark)
      (is (@(resolve test) (@(resolve conversion) 42N))))

    (testing (str test " converts a big decimal to a type of " test-without-question-mark)
      (is (@(resolve test) (@(resolve conversion) 42.2M))))))

(deftest number-conversion-test
  (conversion-test 'integer? 'byte)
  (conversion-test 'integer? 'short)
  (conversion-test 'integer? 'int)
  (conversion-test 'integer? 'long)
  (conversion-test 'float?   'double)
  (conversion-test 'float?   'float)
  (conversion-test 'integer? 'bigint)
  (conversion-test 'integer? 'biginteger)
  (conversion-test 'decimal? 'bigdec))

