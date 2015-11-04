(ns clojure.lang.numbers-test
  (:refer-clojure :only [defmacro let loop doseq defn- deftype if-let if-not when])
  (:require [clojure.test            :refer [deftest is testing]]
            [clojure.lang.numbers    :refer [make-ratio]]
            [clojure.next            :refer :all])
  (:import  [clojure.lang.platform BigInt FallBackNumber]))

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
   (bigint val)     ; clojure.lang.platform.BigInt
   (biginteger val) ; java.math.BigInteger
   ])

(defn- all-floating-types [val]
  [(float val)  ; java.lang.Float
   (double val) ; java.lang.Double
   ])

(deftest number?-test
  (testing "any Number returns true"
    (is (number? (FallBackNumber. 1)))))

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
                                       (bigint 10)     ; clojure.lang.platform.BigInt
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
                                       (bigint 10)        ; clojure.lang.platform.BigInt
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
                                       (bigint 10)     ; clojure.lang.platform.BigInt
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
    (if-not (clojure.core/empty? types)
      (let [test-subject (clojure.core/key (clojure.core/first types))
            t1s (clojure.core/first (clojure.core/val (clojure.core/first types)))
            t2s (clojure.core/second (clojure.core/val (clojure.core/first types)))]
        (doseq [t1 t1s]
          (doseq [t2 t2s]
            (is (= result (operation (t1 left-side) (t2 right-side)))
                (str "operating on " (t1 left-side) " (" (type (t1 left-side)) ") "
                                     (t2 right-side) " (" (type (t2 right-side)) ")"))
            (is (= result (operation (t2 left-side) (t1 right-side)))
                (str "operating on " (t2 left-side) " (" (type (t2 left-side)) ") "
                                     (t1 right-side) " (" (type (t1 right-side)) ")"))
            (is (= test-subject (type (operation (t1 left-side) (t2 right-side))))
                (str "expected type " test-subject " but got " (type (operation (t1 left-side) (t2 right-side)))))
            (is (= test-subject (type (operation (t2 left-side) (t1 right-side))))
                (str "expected type " test-subject " but got " (type (operation (t2 left-side) (t1 right-side))))))))
        (recur (clojure.core/rest types)))))

(defn- number [n] (FallBackNumber. n))

(deftest bit-not-test
  (loop [types [byte short int long]]
    (if-not (clojure.core/empty? types)
      (let [t (clojure.core/first types)]
        (is (= -22 (bit-not (t 21))))
        (is (= Long (type (bit-not (t 21)))))
        (recur (clojure.core/rest types))))))

(deftest bit-and-test
  (op-test {Long [[byte short int long] [byte short int long]]}
           #(bit-and %1 %2)
           4 4 5))

(deftest bit-and-not-test
  (op-test {Long [[byte short int long] [byte short int long]]}
           #(bit-and-not %1 %2)
           2 42 41))

(deftest bit-or-test
  (op-test {Long [[byte short int long] [byte short int long]]}
            #(bit-or %1 %2)
            5 4 5))

(deftest bit-xor-test
  (op-test {Long [[byte short int long] [byte short int long]]}
           #(bit-xor %1 %2)
           1 4 5))

(deftest bit-clear-test
  (op-test {Long [[byte short int long] [byte short int long]]}
           #(bit-clear %1 %2)
           1 5 2))

(deftest bit-set-test
  (op-test {Long [[byte short int long] [byte short int long]]}
           #(bit-set %1 %2)
           3 1 1))

(deftest bit-flip-test
  (op-test {Long [[byte short int long] [byte short int long]]}
           #(bit-flip %1 %2)
           1 0 0))

(deftest bit-test-test
  (op-test {Boolean [[byte short int long] [byte short int long]]}
           #(bit-test %1 %2)
           true 1 0))

(deftest bit-shift-left-test
  (op-test {Long [[byte short int long] [byte short int long]]}
           #(bit-shift-left %1 %2)
           16 2 3))

(deftest bit-shift-right-test
  (op-test {Long [[byte short int long] [byte short int long]]}
           #(bit-shift-right %1 %2)
           1 5 2))

(deftest bit-unsigned-shift-right-test
  (testing "works with a 0 in the signed position"
    (op-test {Long [[byte short int long] [byte short int long]]}
             #(unsigned-bit-shift-right %1 %2)
             2 5 1))

  (testing "works with a 1 in the signed position"
    (op-test {Long [[byte short int long] [byte short int long]]}
             #(unsigned-bit-shift-right %1 %2)
             9223372036854775805 -5 1)))

(deftest integer-addition-test
  (op-test {Long [[int long] [number int long]]
            clojure.lang.platform.BigInt [[bigint biginteger] [number int long bigint biginteger]]}
           #(+ %1 %2)
           3 1 2))

(deftest ratio-addition-test
  (testing "adding an integer ratio to an integer ratio"
    (let [r1 (make-ratio 1 1)
          r2 (make-ratio 2 1)]
      (is (= 3 (+ r1 r2)))
      (is (= clojure.lang.platform.BigInt (type (+ r1 r2))))))

  (testing "adding a decimal ratio to a decimal ratio"
    (let [r1 (make-ratio 1 3)
          r2 (make-ratio 1 3)]
      (is (= (make-ratio 2 3) (+ r1 r2)))))

  (testing "adding a ratio to an int and vica-versa"
    (let [t1 (int 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.BigInt (type (+ t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (+ t2 t1))))
      (is (= 3 (+ t1 t2)))
      (is (= 3 (+ t2 t1)))))

  (testing "adding a ratio to a long and vica-versa"
    (let [t1 (long 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.BigInt (type (+ t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (+ t2 t1))))
      (is (= 3 (+ t1 t2)))
      (is (= 3 (+ t2 t1)))))

  (testing "adding a ratio to a biginteger and vica-versa"
    (let [t1 (biginteger 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.BigInt (type (+ t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (+ t2 t1))))
      (is (= 3 (+ t1 t2)))
      (is (= 3 (+ t2 t1)))))

  (testing "adding a ratio to a bigint and vica-versa"
    (let [t1 (bigint 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.BigInt (type (+ t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (+ t2 t1))))
      (is (= 3 (+ t1 t2)))
      (is (= 3 (+ t2 t1)))))

  (testing "adding a ratio to a bigdecimal and vica-versa"
    (let [t1 (bigdec 0.5)
          t2 (make-ratio 1 2)]
      (is (= BigDecimal (type (+ t1 t2))))
      (is (= BigDecimal (type (+ t2 t1))))
      (is (and (> 1.1 (+ t1 t2)) (< 0.9 (+ t1 t2))))
      (is (and (> 1.1 (+ t2 t1)) (< 0.9 (+ t2 t1))))))

  (testing "adding a ratio to a float and vica-versa"
    (let [t1 (float 0.5)
          t2 (make-ratio 1 2)]
      (is (= Double (type (+ t1 t2))))
      (is (= Double (type (+ t2 t1))))
      (is (and (> 1.1 (+ t1 t2)) (< 0.9 (+ t1 t2))))
      (is (and (> 1.1 (+ t2 t1)) (< 0.9 (+ t2 t1))))))

  (testing "adding a ratio to a double and vica-versa"
    (let [t1 (double 0.5)
          t2 (make-ratio 1 2)]
      (is (= Double (type (+ t1 t2))))
      (is (= Double (type (+ t2 t1))))
      (is (and (> 1.1 (+ t1 t2)) (< 0.9 (+ t1 t2))))
      (is (and (> 1.1 (+ t2 t1)) (< 0.9 (+ t2 t1))))))

  (testing "adding a ratio to a non-specified Number type and vica-versa"
    (let [t1 (number 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.BigInt (type (+ t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (+ t2 t1))))
      (is (= 3 (+ t1 t2)))
      (is (= 3 (+ t2 t1))))))

(deftest big-decimal-addition-test
  (op-test {BigDecimal [[bigdec] [number int long bigint biginteger bigdec]]}
           #(+ %1 %2)
           (bigdec 3.0) 1.0 2.0))

(deftest decimal-addition-test
  (op-test {Double [[double float] [number int long bigint biginteger bigdec double float]]}
           #(+ %1 %2)
           3.0 1.0 2.0))

(deftest decimal-with-decimal-addition-test
  (testing "adding a double to a double"
    (let [t1 (double 1.1)
          t2 (double 2.2)
          result (+ t1 t2)]
      (is (= Double (type result)))
      (is (and (< 3.29 result) (> 3.31 result)))))

  (testing "adding a float to a float"
    (let [t1 (float 1.1)
          t2 (float 2.2)
          result (+ t1 t2)]
      (is (= Double (type result)))
      (is (and (< 3.29 result) (> 3.31 result)))))

  (testing "adding a float to a double and vica-versa"
    (let [t1 (float 1.1)
          t2 (double 2.2)]
      (is (= Double (type (+ t1 t2))))
      (is (= Double (type (+ t2 t1))))
      (let [result1 (+ t1 t2)]
        (is (and (< 3.29 result1) (> 3.31 result1))))
      (let [result2 (+ t2 t1)]
        (is (and (< 3.29 result2) (> 3.31 result2))))))

  (testing "adding a float to a bigdecimal and vica-versa"
    (let [t1 (float 1.1)
          t2 (bigdec 2.2)]
      (is (= Double (type (+ t1 t2))))
      (is (= Double (type (+ t2 t1))))
      (let [result1 (+ t1 t2)]
        (is (and (< 3.29 result1) (> 3.31 result1))))
      (let [result2 (+ t2 t1)]
        (is (and (< 3.29 result2) (> 3.31 result2))))))

  (testing "adding a double to a bigdecimal and vica-versa"
    (let [t1 (double 1.1)
          t2 (bigdec 2.2)]
      (is (= Double (type (+ t1 t2))))
      (is (= Double (type (+ t2 t1))))
      (let [result1 (+ t1 t2)]
        (is (and (< 3.29 result1) (> 3.31 result1))))
      (let [result2 (+ t2 t1)]
        (is (and (< 3.29 result2) (> 3.31 result2)))))))

(deftest long-addition-number-fallback-test
  (testing "falling back to long ops for adding non-covered Numbers"
    (let [t1 (number 1)
          t2 (number 2)]
      (is (= Long (type (+ t1 t2))))
      (is (= 3 (+ t1 t2))))))

(deftest integer-precision-addition-test
  (op-test {Long [[int long] [number int long]]
            clojure.lang.platform.BigInt [[bigint biginteger] [number int long bigint biginteger]]}
           #(+' %1 %2)
           3 1 2))

(deftest integer-precision-upper-overflow-addition-test
  (is (= (BigInt/fromBigInteger (BigInteger. "9223372036854775808")) (+' Long/MAX_VALUE 1))))

(deftest integer-increment-test
  (testing "increment an int"
    (is (= -1 (inc (int -2))))
    (is (= Long (type (inc (int -2))))))

  (testing "increment a long"
    (is (= 3 (inc (long 2))))
    (is (= Long (type (inc (long 2)))))))

(deftest big-int-increment-test
  (testing "incing a big int"
    (is (= (bigint -1) (inc (bigint -2))))
    (is (= clojure.lang.platform.BigInt (type (inc (bigint -2))))))

  (testing "incrementing a big integer"
    (is (= (bigint 3) (inc (biginteger 2))))
    (is (= clojure.lang.platform.BigInt (type (inc (biginteger 2)))))))

(deftest ratio-increment-test
  (testing "incrementing a ratio"
    (is (= (make-ratio 3 2) (inc (make-ratio 1 2))))
    (is (= clojure.lang.platform.Ratio (type (inc (make-ratio 1 2)))))))

(deftest big-decimal-increment-test
  (testing "incrementing a big decimal"
    (is (< 2.1 (inc (bigdec 1.2))))
    (is (> 2.3 (inc (bigdec 1.2))))
    (is (= BigDecimal (type (inc (bigdec 1.2)))))))

(deftest double-increment-test
  (testing "incrementing a float"
    (is (< -1.2 (inc (float -2.1))))
    (is (> -1.0 (inc (float -2.1))))
    (is (= Double (type (inc (float -2.1))))))

  (testing "incrementing a double"
    (is (< 2.1 (inc (double 1.2))))
    (is (> 2.3 (inc (double 1.2))))
    (is (= Double (type (inc (double 1.2)))))))

(deftest number-fallback-increment-test
  (testing "incrementing a number"
    (is (= 3 (inc (number 2))))
    (is (= Long (type (inc (number 2)))))))

(deftest integer-precision-intecrement-overflow-test
  (testing "overflow into BigInt when increment Long/MAX_VALUE"
    (is (= (BigInt/fromBigInteger (BigInteger. "9223372036854775808")) (inc' Long/MAX_VALUE)))))

(deftest integer-multiplication-test
  (op-test {Long [[int long] [number int long]]
            clojure.lang.platform.BigInt [[bigint biginteger] [number int long bigint biginteger]]}
           #(* %1 %2)
           6 2 3))

(deftest ratio-multiplication-test
  (testing "multiplying an integer ratio by an integer ratio"
    (let [r1 (make-ratio 3 1)
          r2 (make-ratio 2 1)]
      (is (= 6 (* r1 r2)))
      (is (= clojure.lang.platform.BigInt (type (* r1 r2))))))

  (testing "multiplying a decimal ratio by a decimal ratio"
    (let [r1 (make-ratio 1 3)
          r2 (make-ratio 1 3)]
      (is (= (make-ratio 1 9) (* r1 r2)))))

  (testing "multiplying a ratio by an int and vica-versa"
    (let [t1 (int 2)
          t2 (make-ratio 3 1)]
      (is (= clojure.lang.platform.BigInt (type (* t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (* t2 t1))))
      (is (= 6 (* t1 t2)))
      (is (= 6 (* t2 t1)))))

  (testing "multiplying a ratio by a long and vica-versa"
    (let [t1 (long 2)
          t2 (make-ratio 3 1)]
      (is (= clojure.lang.platform.BigInt (type (* t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (* t2 t1))))
      (is (= 6 (* t1 t2)))
      (is (= 6 (* t2 t1)))))

  (testing "multiplying a ratio by a biginteger and vica-versa"
    (let [t1 (biginteger 2)
          t2 (make-ratio 3 1)]
      (is (= clojure.lang.platform.BigInt (type (* t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (* t2 t1))))
      (is (= 6 (* t1 t2)))
      (is (= 6 (* t2 t1)))))

  (testing "multiplying a ratio by a bigint and vica-versa"
    (let [t1 (bigint 2)
          t2 (make-ratio 3 1)]
      (is (= clojure.lang.platform.BigInt (type (* t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (* t2 t1))))
      (is (= 6 (* t1 t2)))
      (is (= 6 (* t2 t1)))))

  (testing "multiplying a ratio by a bigdecimal and vica-versa"
    (let [t1 (bigdec 0.5)
          t2 (make-ratio 1 2)]
      (is (= BigDecimal (type (* t1 t2))))
      (is (= BigDecimal (type (* t2 t1))))
      (is (and (> 0.26 (* t1 t2)) (< 0.24 (* t1 t2))))
      (is (and (> 0.26 (* t2 t1)) (< 0.24 (* t2 t1))))))

  (testing "multiplying a ratio by a float and vica-versa"
    (let [t1 (float 0.5)
          t2 (make-ratio 1 2)]
      (is (= Double (type (* t1 t2))))
      (is (= Double (type (* t2 t1))))
      (is (and (> 0.26 (* t1 t2)) (< 0.24 (* t1 t2))))
      (is (and (> 0.26 (* t2 t1)) (< 0.24 (* t2 t1))))))

  (testing "multiplying a ratio by a double and vica-versa"
    (let [t1 (double 0.5)
          t2 (make-ratio 1 2)]
      (is (= Double (type (* t1 t2))))
      (is (= Double (type (* t2 t1))))
      (is (and (> 0.26 (* t1 t2)) (< 0.24 (* t1 t2))))
      (is (and (> 0.26 (* t2 t1)) (< 0.24 (* t2 t1))))))

  (testing "multiplying a ratio by a non-specified Number type and vica-versa"
    (let [t1 (number 2)
          t2 (make-ratio 3 1)]
      (is (= clojure.lang.platform.BigInt (type (* t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (* t2 t1))))
      (is (= 6 (* t1 t2)))
      (is (= 6 (* t2 t1))))))

(deftest big-decimal-multiplication-test
  (op-test {BigDecimal [[bigdec] [number int long bigint biginteger bigdec]]}
           #(* %1 %2)
           (bigdec 6.0) 3.0 2.0))

(deftest decimal-multiplication-test
  (op-test {Double [[double float] [number int long bigint biginteger bigdec double float]]}
           #(* %1 %2)
           6.0 2.0 3.0))

(deftest decimal-with-decimal-multiplication-test
  (testing "multiplying a double by a double"
    (let [t1 (double 1.1)
          t2 (double 2.2)
          result (* t1 t2)]
      (is (= Double (type result)))
      (is (and (< 2.41 result) (> 2.43 result)))))

  (testing "multiplying a float by a float"
    (let [t1 (float 1.1)
          t2 (float 2.2)
          result (* t1 t2)]
      (is (= Double (type result)))
      (is (and (< 2.41 result) (> 2.43 result)))))

  (testing "multiplying a float by a double and vica-versa"
    (let [t1 (float 1.1)
          t2 (double 2.2)]
      (is (= Double (type (* t1 t2))))
      (is (= Double (type (* t2 t1))))
      (let [result1 (* t1 t2)]
        (is (and (< 2.41 result1) (> 2.43 result1))))
      (let [result2 (* t2 t1)]
        (is (and (< 2.41 result2) (> 2.43 result2))))))

  (testing "multiplying a float by a bigdecimal and vica-versa"
    (let [t1 (float 1.1)
          t2 (bigdec 2.2)]
      (is (= Double (type (* t1 t2))))
      (is (= Double (type (* t2 t1))))
      (let [result1 (* t1 t2)]
        (is (and (< 2.41 result1) (> 2.43 result1))))
      (let [result2 (* t2 t1)]
        (is (and (< 2.41 result2) (> 2.43 result2))))))

  (testing "multiplying a double by a bigdecimal and vica-versa"
    (let [t1 (double 1.1)
          t2 (bigdec 2.2)]
      (is (= Double (type (* t1 t2))))
      (is (= Double (type (* t2 t1))))
      (let [result1 (* t1 t2)]
        (is (and (< 2.41 result1) (> 2.43 result1))))
      (let [result2 (* t2 t1)]
        (is (and (< 2.41 result2) (> 2.43 result2)))))))

(deftest long-multiplication-number-fallback-test
  (testing "falling back to long ops for multiplying non-covered Numbers"
    (let [t1 (number 2)
          t2 (number 3)]
      (is (= Long (type (* t1 t2))))
      (is (= 6 (* t1 t2))))))

(deftest integer-precision-multiplication-test
  (op-test {Long [[int long] [number int long]]}
            #(*' %1 %2)
            8 4 2))

(deftest integer-precision-upper-overflow-multiplicaton-test
  (is (= (BigInt/fromBigInteger (BigInteger. "9223372036854775808"))  (*' 4611686018427387904 2))))

(deftest integer-precision-lower-overflow-multiplication-test
  (is (= (BigInt/fromBigInteger (BigInteger. "-9223372036854775810")) (*' -4611686018427387905 2))))

(deftest integer-subtraction-test
  (op-test {Long [[int long] [number int long]]
            clojure.lang.platform.BigInt [[bigint biginteger] [number int long bigint biginteger]]}
           #(- %1 %2)
           6 10 4))

(deftest single-integer-subtraction-test
  (loop [types [int long bigint biginteger number]]
    (if-not (clojure.core/empty? types)
      (let [t (clojure.core/first types)]
        (is (clojure.core/== (t -1) (- (t 1))))
        (recur (clojure.core/rest types))))))

(deftest ratio-subtraction-test
  (testing "subtracting an integer ratio from an integer ratio"
    (let [r1 (make-ratio 10 1)
          r2 (make-ratio 5 1)]
      (is (= clojure.lang.platform.BigInt (type (- r1 r2))))
      (is (= 5 (- r1 r2)))))

  (testing "subtracting a decimal from a ratio by a decimal ratio"
    (let [r1 (make-ratio 1 4)
          r2 (make-ratio 1 8)]
      (is (= clojure.lang.platform.Ratio (type (- r1 r2))))
      (is (= (make-ratio 1 8) (- r1 r2)))))

  (testing "subtracting a ratio by an int and vica-versa"
    (let [t1 (int 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.BigInt (type (- t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (- t2 t1))))
      (is (= -1 (- t1 t2)))
      (is (= 1 (- t2 t1)))))

  (testing "subtracting a ratio by a long and vica-versa"
    (let [t1 (long 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.BigInt (type (- t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (- t2 t1))))
      (is (= -1 (- t1 t2)))
      (is (= 1 (- t2 t1)))))

  (testing "subtracting a ratio by a biginteger and vica-versa"
    (let [t1 (biginteger 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.BigInt (type (- t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (- t2 t1))))
      (is (= -1 (- t1 t2)))
      (is (= 1 (- t2 t1)))))

  (testing "subtracting a ratio by a bigint and vica-versa"
    (let [t1 (bigint 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.BigInt (type (- t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (- t2 t1))))
      (is (= -1 (- t1 t2)))
      (is (= 1 (- t2 t1)))))

  (testing "subtracting a ratio by a bigdecimal and vica-versa"
    (let [t1 (bigdec 3.0)
          t2 (make-ratio 1 2)
          result (- t1 t2)]
      (is (= BigDecimal (type result)))
      (is (< result 2.51))
      (is (> result 2.49)))
    (let [t1 (make-ratio 3 1)
          t2 (bigdec 0.5)
          result (- t1 t2)]
      (is (= BigDecimal (type result)))
      (is (< result 2.51))
      (is (> result 2.49))))

  (testing "subtracting a ratio by a float and vica-versa"
    (let [t1 (float 3.0)
          t2 (make-ratio 1 2)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 2.51))
      (is (> result 2.49)))
    (let [t1 (make-ratio 3 1)
          t2 (float 0.5)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 2.51))
      (is (> result 2.49))))

  (testing "subtracting a ratio by a double and vica-versa"
    (let [t1 (double 3.0)
          t2 (make-ratio 1 2)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 2.51))
      (is (> result 2.49)))
    (let [t1 (make-ratio 3 1)
          t2 (double 0.5)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 2.51))
      (is (> result 2.49))))

  (testing "subtracting a ratio by a non-specified Number type and vica-versa"
    (let [t1 (number 1)
          t2 (make-ratio 3 2)]
      (is (= clojure.lang.platform.Ratio (type (- t1 t2))))
      (is (= clojure.lang.platform.Ratio (type (- t2 t1))))
      (is (= (make-ratio -1 2) (- t1 t2)))
      (is (= (make-ratio 1 2) (- t2 t1))))))

(deftest single-ratio-subtraction-test
  (is (= (make-ratio -2 3) (- (make-ratio 2 3)))))

(deftest big-decimal-subtraction-test
  (op-test {BigDecimal [[bigdec] [number int long bigint biginteger bigdec]]}
           #(- %1 %2)
           (bigdec 1.0) 3.0 2.0))

(deftest decimal-subtraction-test
  (op-test {Double [[double float] [number int long bigint biginteger bigdec double float]]}
           #(- %1 %2)
           1.0 3.0 2.0))

(deftest decimal-with-decimal-subtraction-test
  (testing "subtracting a double by a double"
    (let [t1 (double 2.1)
          t2 (double 1.2)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.91))
      (is (> result 0.89))))

  (testing "subtracting a float by a float"
    (let [t1 (float 2.1)
          t2 (float 1.2)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.91))
      (is (> result 0.89))))

  (testing "subtacting a float by a double and vica-versa"
    (let [t1 (float 2.1)
          t2 (double 1.2)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.91))
      (is (> result 0.89)))
    (let [t1 (double 2.1)
          t2 (float 1.2)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.91))
      (is (> result 0.89))))

  (testing "subtracting a float by a bigdecimal and vica-versa"
    (let [t1 (float 2.1)
          t2 (bigdec 1.2)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.91))
      (is (> result 0.89)))
    (let [t1 (bigdec 2.1)
          t2 (float 1.2)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.91))
      (is (> result 0.89))))

  (testing "dividng a double by a bigdecimal and vica-versa"
    (let [t1 (double 2.1)
          t2 (bigdec 1.2)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.91))
      (is (> result 0.89)))
    (let [t1 (bigdec 2.1)
          t2 (double 1.2)
          result (- t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.91))
      (is (> result 0.89)))))

(deftest long-subtraction-number-fallback-test
  (testing "falling back to long ops for subtracting non-covered Numbers"
    (let [t1 (number 10)
          t2 (number 5)]
      (is (= Long (type (- t1 t2))))
      (is (= 5 (- t1 t2))))))

(deftest integer-precision-negation-subtraction-test
  (testing "overflow an integer into a BigInt by negating Long/MIN_VALUE"
    (is (= (BigInt/fromBigInteger (BigInteger. "9223372036854775808")) (-' Long/MIN_VALUE)))))

(deftest integer-precision-subtraction-test
  (testing "overflow an integer via min value"
    (is (= (BigInt/fromBigInteger (BigInteger. "-9223372036854775809")) (-' Long/MIN_VALUE 1))))):w

(deftest integer-decrement-test
  (testing "decrement an int"
    (is (= -3 (dec (int -2))))
    (is (= Long (type (dec (int -2))))))

  (testing "decrement a long"
    (is (= 1 (dec (long 2))))
    (is (= Long (type (dec (long 2)))))))

(deftest big-int-decrement-test
  (testing "decrementing a big int"
    (is (= (bigint -3) (dec (bigint -2))))
    (is (= clojure.lang.platform.BigInt (type (dec (bigint -2))))))

  (testing "decrementing a big integer"
    (is (= (bigint 1) (dec (biginteger 2))))
    (is (= clojure.lang.platform.BigInt (type (dec (biginteger 2)))))))

(deftest ratio-decrement-test
  (testing "decrementing a ratio"
    (is (= (make-ratio 1 2) (dec (make-ratio 3 2))))
    (is (= clojure.lang.platform.Ratio (type (dec (make-ratio 3 2)))))))

(deftest big-decimal-decrementing-test
  (testing "decrementing a big decimal"
    (is (< 0.1 (dec (bigdec 1.2))))
    (is (> 0.3 (dec (bigdec 1.2))))
    (is (= BigDecimal (type (dec (bigdec 1.2)))))))

(deftest double-decrement-test
  (testing "decrementing a float"
    (is (< -3.2 (dec (float -2.1))))
    (is (> -3.0 (dec (float -2.1))))
    (is (= Double (type (dec (float -2.1))))))

  (testing "decrementing a double"
    (is (< 0.1 (dec (double 1.2))))
    (is (> 0.3 (dec (double 1.2))))
    (is (= Double (type (dec (double 1.2)))))))

(deftest number-fallback-decrement-test
  (testing "decrement a number"
    (is (= 1 (dec (number 2))))
    (is (= Long (type (dec (number 2)))))))

(deftest integer-precision-decrement-overflow-test
  (testing "overflows into BigInt when decrementing Long/MIN_VALUE"
    (is (= (BigInt/fromBigInteger (BigInteger. "-9223372036854775809")) (dec' Long/MIN_VALUE)))))

(deftest integer-division-test
  (op-test {Long [[int long] [number int long]]
            clojure.lang.platform.BigInt [[bigint biginteger] [number int long bigint biginteger]]}
           #(/ %1 %2)
           2 10 5))

(deftest ratio-division-test
  (testing "dividing an integer by a ratio by an integer ratio"
    (let [r1 (make-ratio 10 1)
          r2 (make-ratio 5 1)]
      (is (= clojure.lang.platform.BigInt (type (/ r1 r2))))
      (is (= 2 (/ r1 r2)))))

  (testing "dividing a decimal by a ratio to a decimal ratio"
    (let [r1 (make-ratio 1 3)
          r2 (make-ratio 1 4)]
      (is (= clojure.lang.platform.Ratio (type (/ r1 r2))))
      (is (= (make-ratio 4 3) (/ r1 r2)))))

  (testing "dividing a ratio by an int and vica-versa"
    (let [t1 (int 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.Ratio (type (/ t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (/ t2 t1))))
      (is (= (make-ratio 1 2) (/ t1 t2)))
      (is (= 2 (/ t2 t1)))))

  (testing "dividing a ratio by a long and vica-versa"
    (let [t1 (long 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.Ratio (type (/ t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (/ t2 t1))))
      (is (= (make-ratio 1 2) (/ t1 t2)))
      (is (= 2 (/ t2 t1)))))

  (testing "dividing a ratio by a biginteger and vica-versa"
    (let [t1 (biginteger 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.Ratio (type (/ t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (/ t2 t1))))
      (is (= (make-ratio 1 2) (/ t1 t2)))
      (is (= 2 (/ t2 t1)))))

  (testing "dividing a ratio by a bigint and vica-versa"
    (let [t1 (bigint 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.Ratio (type (/ t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (/ t2 t1))))
      (is (= (make-ratio 1 2) (/ t1 t2)))
      (is (= 2 (/ t2 t1)))))

  (testing "dividing a ratio by a bigdecimal and vica-versa"
    (let [t1 (bigdec 0.4)
          t2 (make-ratio 5 2)
          result (/ t1 t2)]
      (is (= BigDecimal (type result)))
      (is (< result 0.161))
      (is (> result 0.159)))
    (let [t1 (make-ratio 2 5)
          t2 (bigdec 2.5)
          result (/ t1 t2)]
      (is (= BigDecimal (type result)))
      (is (< result 0.161))
      (is (> result 0.159))))

  (testing "dividing a ratio by a float and vica-versa"
    (let [t1 (float 0.4)
          t2 (make-ratio 5 2)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.161))
      (is (> result 0.159)))
    (let [t1 (make-ratio 2 5)
          t2 (float 2.5)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.161))
      (is (> result 0.159))))

  (testing "dividing a ratio by a double and vica-versa"
    (let [t1 (double 0.4)
          t2 (make-ratio 5 2)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.161))
      (is (> result 0.159)))
    (let [t1 (make-ratio 2 5)
          t2 (double 2.5)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 0.161))
      (is (> result 0.159))))

  (testing "dividng a ratio by a non-specified Number type and vica-versa"
    (let [t1 (number 1)
          t2 (make-ratio 2 1)]
      (is (= clojure.lang.platform.Ratio (type (/ t1 t2))))
      (is (= clojure.lang.platform.BigInt (type (/ t2 t1))))
      (is (= (make-ratio 1 2) (/ t1 t2)))
      (is (= 2 (/ t2 t1))))))

(deftest big-decimal-division-test
  (op-test {BigDecimal [[bigdec] [number int long bigint biginteger bigdec]]}
           #(/ %1 %2)
           (bigdec 1.0) 2.0 2.0))

(deftest decimal-division-test
  (op-test {Double [[double float] [number int long bigint biginteger bigdec double float]]}
           #(/ %1 %2)
           1.0 2.0 2.0))

(deftest decimal-with-decimal-division-test
  (testing "dividing a double by a double"
    (let [t1 (double 2.1)
          t2 (double 1.2)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 1.76))
      (is (> result 1.74))))

  (testing "dividing a float by a float"
    (let [t1 (float 2.1)
          t2 (float 1.2)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 1.76))
      (is (> result 1.74))))

  (testing "dividing a float by a double and vica-versa"
    (let [t1 (float 2.1)
          t2 (double 1.2)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 1.76))
      (is (> result 1.74)))
    (let [t1 (double 2.1)
          t2 (float 1.2)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 1.76))
      (is (> result 1.74))))

  (testing "dividng a float by a bigdecimal and vica-versa"
    (let [t1 (float 2.1)
          t2 (bigdec 1.2)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 1.76))
      (is (> result 1.74)))
    (let [t1 (bigdec 2.1)
          t2 (float 1.2)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 1.76))
      (is (> result 1.74))))

  (testing "dividng a double by a bigdecimal and vica-versa"
    (let [t1 (double 2.1)
          t2 (bigdec 1.2)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 1.76))
      (is (> result 1.74)))
    (let [t1 (bigdec 2.1)
          t2 (double 1.2)
          result (/ t1 t2)]
      (is (= Double (type result)))
      (is (< result 1.76))
      (is (> result 1.74)))))

(deftest long-division-number-fallback-test
  (testing "falling back to long ops for dividing non-covered Numbers"
    (let [t1 (number 10)
          t2 (number 5)]
      (is (= Long (type (/ t1 t2))))
      (is (= 2 (/ t1 t2))))))

(deftest max-fallback-number-test
  (testing "max can handle arbitrary number types"
    (let [t1 (number 5)
          t2 (number 6)]
      (is (= FallBackNumber (type (max t1 t2))))
      (is (= 6 (.longValue (max t1 t2)))))))

(deftest max-NaN-test
  (testing "max will always return NaN if present"
    (is (. Double isNaN (max 1 Double/NaN 2)))))

(deftest min-fallback-number-test
  (testing "min can handle arbitrary number types"
    (let [t1 (number 5)
          t2 (number 6)]
      (is (= FallBackNumber (type (min t1 t2))))
      (is (= 5 (.longValue (min t1 t2)))))))

(deftest min-NaN-test
  (testing "min will always return NaN if presesnt"
    (is (. Double isNaN (min 1 Double/NaN 2)))))

(deftest integer-zero-test
  (is (zero? (int 0)))
  (is (not (zero? (int 1))))
  (is (zero? (long 0)))
  (is (not (zero? (int 1)))))

(deftest big-integer-zero-test
  (is (zero? (bigint 0)))
  (is (not (zero? (bigint 1))))
  (is (zero? (biginteger 0)))
  (is (not (zero? (biginteger 1)))))

(deftest ratio-zero-test
  (is (zero? (make-ratio 0 1)))
  (is (not (zero? (make-ratio 1 1)))))

(deftest big-decimal-zero-test
  (is (zero? (bigdec 0.0)))
  (is (not (zero? (bigdec 1.0)))))

(deftest double-zero-test
  (is (zero? (double 0.0)))
  (is (not (zero? (double 1.0))))
  (is (zero? (float 0.0)))
  (is (not (zero? (float 1.0)))))

(deftest ratio-test
  (testing "numerator of a ratio"
    (let [ratio (make-ratio 1 2)]
      (is (= 1 (numerator ratio)))))

  (testing "denominator of a ratio"
    (let [ratio (make-ratio 1 2)]
      (is (= 2 (denominator ratio))))))
