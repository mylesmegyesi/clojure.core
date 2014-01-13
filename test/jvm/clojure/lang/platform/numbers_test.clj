(ns clojure.lang.platform.numbers-test
  (:refer-clojure :only [defmacro let doseq defn cons
                         byte short int long bigint biginteger float double
                         = / not=
                         ])
  (:require [clojure.test                 :refer [deftest is testing]]
            [clojure.lang.show            :refer [str]]
            [clojure.lang.platform.object :refer [type]]))

(defmacro all-pairs-equal [equal-var vals]
  `(let [equal-var# ~equal-var
         vals# ~vals]
     (doseq [val1# vals#]
       (doseq [val2# vals#]
         (is (equal-var# val1# val2#)
             (str "Test that " val1# " (" (type val1#) ") "
                  equal-var# " " val2# " (" (type val2#) ")"))))))

(defn all-integer-types [val]
  [(byte val)       ; java.lang.Byte
   (short val)      ; java.lang.Short
   (int val)        ; java.lang.Integer
   (long val)       ; java.lang.Long
   (bigint val)     ; clojure BigInt
   (biginteger val) ; java.math.BigInteger
   ])

(defn all-floating-types [val]
  [(float val)  ; java.lang.Float
   (double val) ; java.lang.Double
   ])

(deftest =-test

  ; integer types

  (testing "integer types are equivalent"
    (all-pairs-equal #'= (all-integer-types 2)))

  (testing "integer types are not equivalent to floating types, even when their value is similar"
    (is (not= 0 0.0))
    (is (not= 0.0 0)))

  (testing "integer types are not equivalent to decimal types, even when their value is similar"
    (is (not= 0 0.0M))
    (is (not= 0.0M 0)))

  ; floating types

  (testing "floating types are equivalent"
    (all-pairs-equal #'= (all-floating-types 2.0))
    (all-pairs-equal #'= (all-floating-types 1.5)))

  (testing "floating types are not equivalent to decimal types"
    (is (not= 0.0 0.0M)))

  (testing "floating types are not equivalent to ratios"
    (is (not= (/ 3 2) (float 1.5)))
    (is (not= (/ 3 2) (double 1.5)))
    (is (not= (float 1.5) (/ 3 2)))
    (is (not= (double 1.5) (/ 3 2))))

  ; decimal types

  (testing "decimal types are equivalent"
    (all-pairs-equal #'= [2.0M 2.0M])
    (all-pairs-equal #'= [1.50M 1.50M]))

  (testing "decimal types that differ in scale are still equivalent with ="
    (all-pairs-equal #'= [2.0M 2.00M])
    (all-pairs-equal #'= [1.50M 1.500M])
    (all-pairs-equal #'= [0.0M 0.00M]))

  ; ratios

  (testing "ratios are equivalent"
    (all-pairs-equal #'= [(/ 1 2) (/ 2 4)]))

  (testing "ratios are equivalent to integer types when the resulting value is an integer"
    (all-pairs-equal #'= (cons (/ 9 3) (all-integer-types 3)))))
