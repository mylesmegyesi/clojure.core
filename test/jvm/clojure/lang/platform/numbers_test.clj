(ns clojure.lang.platform.numbers-test
  (:refer-clojure :only [defmacro let doseq defn cons when
                         byte short int long bigint biginteger float double
                         = == / not= hash
                         ])
  (:require [clojure.test                 :refer [deftest is testing]]
            ; [clojure.lang.hash            :refer [hash]] make me work
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


(defn all-pairs-hash-consistent-with-= [vals]
  (doseq [val1 vals]
    (doseq [val2 vals]
      (when (= val1 val2)
        (is (= (hash val1) (hash val2))
            (str "Test that (hash " val1 ") (" (type val1) ") "
                 " = (hash " val2 ") (" (type val2) ")"))))))

(defn all-integer-types [val]
  [(byte val)       ; java.lang.Byte
   (short val)      ; java.lang.Short
   (int val)        ; java.lang.Integer
   (long val)       ; java.lang.Long
   (bigint val)     ; clojure.lang.BigInt
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

(deftest ==-test
  (testing "== tests for *numerical equality* - all types of numbers are loosely equal to each other"
    (all-pairs-equal #'== [(byte 0) (short 0) (int 0) (long 0)
                           (bigint 0) (biginteger 0)
                           (float 0.0) (double 0.0) 0.0M 0.00M])
    (all-pairs-equal #'== [(byte 2) (short 2) (int 2) (long 2)
                           (bigint 2) (biginteger 2)
                           (float 2.0) (double 2.0) 2.0M 2.00M])
    (all-pairs-equal #'== [(/ 3 2) (float 1.5) (double 1.5) 1.50M 1.500M])))

;; No BigIntegers or floats in following tests, because of (CLJ-1036).
;; queue violin ... :(
(deftest hash-test

  (testing "integer types that are equal also have the same hash code"
    (all-pairs-hash-consistent-with-= [(byte 10)       ; java.lang.Byte
                                       (short 10)      ; java.lang.Short
                                       (int 10)        ; java.lang.Integer
                                       (long 10)       ; java.lang.Long
                                       (bigint 10)     ; clojure.lang.BigInt
                                       ; (biginteger val) ; java.math.BigInteger
                                       ])
    ; maybe some day ...
    ; (all-pairs-hash-consistent-with-= (all-integer-types 10))
      )

  (testing "floating types that are equal also have the same hash code"
    (all-pairs-hash-consistent-with-= [(double 1.5)  ; java.lang.Double
                                       (double 1.5)  ; java.lang.Double
                                       ; (float 1.5) ; java.lang.Float
                                       ])
    ; maybe some day ...
    ; (all-pairs-hash-consistent-with-= [(double 1.5) (float 1.5)])
    )

  (testing "ratio types that are equal also have the same hash code"
    (all-pairs-hash-consistent-with-= [(/ 3 2) ; clojure.lang.Ratio
                                       (/ 3 2)]))

  (testing "decimal types that are equal also have the same hash code"
    (all-pairs-hash-consistent-with-= [0.0M 0.00M ; java.math.BigDeciaml
                                       ]))

  (testing "floating types that are *numerically equivalent* to integer types have the same hash code"
    (all-pairs-hash-consistent-with-= [(byte 10)          ; java.lang.Byte
                                       (short 10)         ; java.lang.Short
                                       (int 10)           ; java.lang.Integer
                                       (long 10)          ; java.lang.Long
                                       (bigint 10)        ; clojure.lang.BigInt
                                       ; (biginteger val) ; java.math.BigInteger
                                       ; (float 10.0)     ; java.lang.Float
                                       (double 10.0)      ; java.lang.Double
                                       ])
    ; maybe some day ...
    ; (all-pairs-hash-consistent-with-= (concat (all-integer-types 10)
    ;                                           (all-floating-types 10.0)))
    )

  (testing "floating types that are *numerically equivalent* to ratio types have the same hash code"
    (all-pairs-hash-consistent-with-= [(/ 3 2)      ; clojure.lang.Ratio
                                       (double 1.5) ; java.lang.Double
                                       ])
    ; maybe some day ...
    ; (all-pairs-hash-consistent-with-= (conj (all-floating-types 1.5)
    ;                                         (/ 3 2)))
    )

  (testing "floating types that are *numerically equivalent* to decimal types have the same hash code"
    (all-pairs-hash-consistent-with-= [1.5M 1.50M   ; java.math.BigDeciaml
                                       (double 1.5) ; java.lang.Double
                                       ]))

  (testing "ratio types that are *numerically equivalent* to decimal types have the same hash code"
    (all-pairs-hash-consistent-with-= [1.5M 1.50M   ; java.math.BigDeciaml
                                       (/ 3 2)      ; clojure.lang.Ratio
                                       ]))

  (testing "decimal types that are *numerically equivalent* to integer types have the same hash code"
    (all-pairs-hash-consistent-with-= [(byte 10)       ; java.lang.Byte
                                       (short 10)      ; java.lang.Short
                                       (int 10)        ; java.lang.Integer
                                       (long 10)       ; java.lang.Long
                                       (bigint 10)     ; clojure.lang.BigInt
                                       ; (biginteger val) ; java.math.BigInteger
                                       10.0M
                                       10.00M])
    ; maybe some day ...
    ; (all-pairs-hash-consistent-with-= (concat (all-integer-types 10.0)
    ;                                           [10.0M 10.00M]))
    )

  )
