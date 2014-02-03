(ns clojure.lang.assertions
  (:refer-clojure :only [defmacro sort])
  (:require [clojure.test            :refer [is]]
            [clojure.lang.comparison :refer [compare]]
            [clojure.next            :refer :all]))

(defmacro is-less-than [lhs rhs]
  `(do
     (is (= -1 (compare ~lhs ~rhs)))
     (is (= [~lhs ~rhs] (sort [~rhs ~lhs])))
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
     (is (= [~rhs ~lhs] (sort [~lhs ~rhs])))
     ;(is (not<  lhs rhs))
     ;(is (not<= lhs rhs))
     ;(is (not=  lhs rhs))
     ;(is (>=    lhs rhs))
     ;(is (>     lhs rhs))
     ))
