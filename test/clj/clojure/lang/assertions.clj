(ns clojure.lang.assertions
  (:refer-clojure :only [defmacro])
  (:require [clojure.test :refer [is]]
            [clojure.next :refer :all]))

(defmacro is-less-than [lhs rhs]
  `(do
     (is (= -1 (compare ~lhs ~rhs)))
     (is (= (sort (vector ~rhs ~lhs)) (vector ~lhs ~rhs)))
     ;(is (<     lhs rhs))
     ;(is (<=    lhs rhs))
     ;(is (not=  lhs rhs))
     ;(is (not>= lhs rhs))
     ;(is (not>  lhs rhs))
     ))

(defmacro is-equal [lhs rhs]
  `(do
     (is (= 0 (compare ~lhs ~rhs)))
     ;(is (not< lhs rhs))
     ;(is (<=   lhs rhs))
     ;(is (=    lhs rhs))
     ;(is (>=   lhs rhs))
     ;(is (not> lhs rhs))
     ))

(defmacro is-greater-than [lhs rhs]
  `(do
     (is (= 1 (compare ~lhs ~rhs)))
     (is (= (sort (vector ~lhs ~rhs)) (vector ~rhs ~lhs)))
     ;(is (not<  lhs rhs))
     ;(is (not<= lhs rhs))
     ;(is (not=  lhs rhs))
     ;(is (>=    lhs rhs))
     ;(is (>     lhs rhs))
     ))
