(ns clojure.lang.numbers-test
  (:refer-clojure :only [defmacro let loop doseq defn- deftype if-let when
                         byte short int long bigint biginteger float double
                         / < >
                         ])
  (:require [clojure.test            :refer [deftest is testing]]
            [clojure.lang.object     :refer [type]]
            [clojure.lang.numbers    :refer :all]
            [clojure.next            :refer :all])
  (:import  [clojure.lang.platform FallBackNumber]))

(defmacro all-pairs-equal [equal-var vals]
  `(let [equal-var# ~equal-var
         vals# ~vals]
     (doseq [val1# vals#]
       (doseq [val2# vals#]
         (is (equal-var# val1# val2#)
             (str "Test that " val1# " (" (type val1#) ") "
                  equal-var# " " val2# " (" (type val2#) ")"))))))


(defn- all-pairs-hash-consistent-with-= [vals]
  (doseq [val1 vals]
    (doseq [val2 vals]
      (when (= val1 val2)
        (is (= (hash val1) (hash val2))
            (str "Test that (hash " val1 ") (" (type val1) ") "
                 " = (hash " val2 ") (" (type val2) ")"))))))

(defn- all-integer-types [val]
  [(byte val)       ; java.lang.Byte
   (short val)      ; java.lang.Short
   (int val)        ; java.lang.Integer
   (long val)       ; java.lang.Long
   (bigint val)     ; clojure.lang.BigInt
   (biginteger val) ; java.math.BigInteger
   ])

(defn- all-floating-types [val]
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
    (is (not= (make-ratio 3 2) (float 1.5)))
    (is (not= (make-ratio 3 2) (double 1.5)))
    (is (not= (float 1.5) (make-ratio 3 2)))
    (is (not= (double 1.5) (make-ratio 3 2))))

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
    (all-pairs-equal #'= [(make-ratio 1 2) (make-ratio 2 4)]))

  ;(testing "division is equivalent to integer types when the resulting value is an integer"
  ;  (all-pairs-equal #'= (cons (make-ratio 9 3) (all-integer-types 3))))

  )


(deftest ==-test
  (testing "== tests for *numerical equality* - all types of numbers are loosely equal to each other"
    (all-pairs-equal #'== [(byte 0) (short 0) (int 0) (long 0)
                           (bigint 0) (biginteger 0)
                           (float 0.0) (double 0.0) 0.0M 0.00M])
    (all-pairs-equal #'== [(byte 2) (short 2) (int 2) (long 2)
                           (bigint 2) (biginteger 2)
                           (float 2.0) (double 2.0) 2.0M 2.00M])
    (all-pairs-equal #'== [(make-ratio 3 2) (float 1.5) (double 1.5) 1.50M 1.500M])))

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
    (all-pairs-hash-consistent-with-= [(make-ratio 3 2)    ; clojure.lang.ratio.Ratio
                                       (make-ratio 3 2)]))

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
    (all-pairs-hash-consistent-with-= [(make-ratio 3 2) ; clojure.lang.ratio.Ratio
                                       (double 1.5)     ; java.lang.Double
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
    (all-pairs-hash-consistent-with-= [1.5M 1.50M       ; java.math.BigDeciaml
                                       (make-ratio 3 2) ; clojure.lang.ratio.Ratio
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

(defn- op-test [types-to-op operation result left-side right-side]
  (loop [types types-to-op]
    (if (not (clojure.core/empty? types))
      (let [test-subject (clojure.core/key (clojure.core/first types))
            t1s (clojure.core/first (clojure.core/val (clojure.core/first types)))
            t2s (clojure.core/second (clojure.core/val (clojure.core/first types)))]
        (doseq [t1 t1s]
          (doseq [t2 t2s]
            (is (= result (operation (t1 left-side) (t2 right-side)))
                (str "adding " (t1 left-side) " (" (type (t1 left-side)) ") "
                               (t2 right-side) " (" (type (t2 right-side)) ")"))
            (is (= result (operation (t2 left-side) (t1 right-side)))
                (str "adding " (t2 left-side) " (" (type (t2 left-side)) ") "
                               (t1 right-side) " (" (type (t1 right-side)) ")"))
            (is (= test-subject (type (operation (t1 left-side) (t2 right-side))))
                (str "expected type " test-subject " but got " (type (operation (t1 left-side) (t2 right-side)))))
            (is (= test-subject (type (operation (t2 left-side) (t1 right-side))))
                (str "expected type " test-subject " but got " (type (operation (t2 left-side) (t1 right-side))))))))
        (recur (clojure.core/rest types)))))

(defn- bigdecimal [n] (BigDecimal. n))
(defn- number [n] (FallBackNumber. n))

(deftest integer-addition-test
  (op-test {Long [[int long] [number int long]]
            clojure.lang.BigInt [[bigint biginteger] [number int long bigint biginteger]]}
           add
           3 1 2))

(deftest ratio-addition-test
  (testing "adding an integer ratio to an integer ratio"
    (let [r1 (make-ratio 1 1)
          r2 (make-ratio 2 1)]
      (is (= 3 (add r1 r2)))
      (is (= clojure.lang.BigInt (type (add r1 r2))))))

  (testing "adding a decimal ratio to a decimal ratio"
    (let [r1 (make-ratio 1 3)
          r2 (make-ratio 1 3)]
      (is (= (make-ratio 2 3) (add r1 r2)))))

  (testing "adding a ratio to an int and vica-versa"
    (let [t1 (int 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.BigInt (type (add t1 t2))))
      (is (= clojure.lang.BigInt (type (add t2 t1))))
      (is (= 3 (add t1 t2)))
      (is (= 3 (add t2 t1)))))

  (testing "adding a ratio to a long and vica-versa"
    (let [t1 (long 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.BigInt (type (add t1 t2))))
      (is (= clojure.lang.BigInt (type (add t2 t1))))
      (is (= 3 (add t1 t2)))
      (is (= 3 (add t2 t1)))))

  (testing "adding a ratio to a biginteger and vica-versa"
    (let [t1 (biginteger 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.BigInt (type (add t1 t2))))
      (is (= clojure.lang.BigInt (type (add t2 t1))))
      (is (= 3 (add t1 t2)))
      (is (= 3 (add t2 t1)))))

)

(deftest big-decimal-addition-test
  (op-test {BigDecimal [[bigdecimal] [number int long bigint biginteger bigdecimal]]}
           add
           (bigdecimal 3.0) 1.0 2.0))

(deftest decimal-addition-test
  (op-test {Double [[double float] [number int long bigint biginteger bigdecimal double float]]}
           add
           3.0 1.0 2.0))

(deftest bigdecimal-addition-to-bigdecimal-test
  (testing "adding a bigdecimal to a bigdecimal"
    (let [t1 (bigdecimal 1.1)
          t2 (bigdecimal 2.2)
          result (add t1 t2)]
      (is (= BigDecimal (type result)))
      (is (and (< 3.29 result) (> 3.31 result))))))

(deftest decimal-with-decimal-addition-test
  (testing "adding a double to a double"
    (let [t1 (double 1.1)
          t2 (double 2.2)
          result (add t1 t2)]
      (is (= Double (type result)))
      (is (and (< 3.29 result) (> 3.31 result)))))

  (testing "adding a float to a float"
    (let [t1 (float 1.1)
          t2 (float 2.2)
          result (add t1 t2)]
      (is (= Double (type result)))
      (is (and (< 3.29 result) (> 3.31 result)))))

  (testing "adding a float to a double and vica-versa"
    (let [t1 (float 1.1)
          t2 (double 2.2)]
      (is (= Double (type (add t1 t2))))
      (is (= Double (type (add t2 t1))))
      (let [result1 (add t1 t2)]
        (is (and (< 3.29 result1) (> 3.31 result1))))
      (let [result2 (add t2 t1)]
        (is (and (< 3.29 result2) (> 3.31 result2))))))

  (testing "adding a float to a bigdecimal and vica-versa"
    (let [t1 (float 1.1)
          t2 (bigdecimal 2.2)]
      (is (= Double (type (add t1 t2))))
      (is (= Double (type (add t2 t1))))
      (let [result1 (add t1 t2)]
        (is (and (< 3.29 result1) (> 3.31 result1))))
      (let [result2 (add t2 t1)]
        (is (and (< 3.29 result2) (> 3.31 result2))))))

  (testing "adding a double to a bigdecimal and vica-versa"
    (let [t1 (double 1.1)
          t2 (bigdecimal 2.2)]
      (is (= Double (type (add t1 t2))))
      (is (= Double (type (add t2 t1))))
      (let [result1 (add t1 t2)]
        (is (and (< 3.29 result1) (> 3.31 result1))))
      (let [result2 (add t2 t1)]
        (is (and (< 3.29 result2) (> 3.31 result2)))))))

(deftest long-addition-number-fallback-test
  (testing "falling back to long ops for non-covered Numbers"
    (let [t1 (number 1)
          t2 (number 2)]
      (is (= Long (type (add t1 t2))))
      (is (= 3 (add t1 t2))))))

(deftest integer-division-test
  (op-test {Long [[int long] [int long]]
            clojure.lang.BigInt [[bigint biginteger] [int long bigint biginteger]]}
           divide
           2 10 5))

(deftest ratio-test
  (testing "numerator of a ratio"
    (let [ratio (make-ratio 1 2)]
      (is (= 1 (numerator ratio)))))

  (testing "denominator of a ratio"
    (let [ratio (make-ratio 1 2)]
      (is (= 2 (denominator ratio))))))
