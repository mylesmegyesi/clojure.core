(ns clojure.ruby.compiler-test
  (:refer-clojure :exclude [compile])
  (:require
    [clojure.test          :refer :all]
    [clojure.ruby.compiler :as    compiler]))

(def empty-env
  {:context :expr
   :locals {}
   :namespaces (atom {})})

;(defn- compile [string]
;  (compiler/compile string empty-env))
;
;(deftest clojure.ruby.compiler
;  (testing "integers"
;    (is "1"
;        (compile "1"))
;    (is "1000000000"
;        (compile "1000000000")))
;
;  (testing "floats"
;    (is "1.5"
;        (compile "1.5"))
;    (is "1.6"
;        (compile "1.6")))
;
;  (testing "strings"
;    (is "\"this is a string\""
;        (compile "\"this is a string\"")))
;
;  (testing "booleans"
;    (is "true"
;        (compile "true"))
;    (is "false"
;        (compile "false")))
;
;  (testing "if statement"
;    (is "if true\n1\n2"
;        (compile "(if true 1 2)"))
;    )
;
;  (testing "=*"
;    ;(is "true"
;    ;    (compile "(=* 1)"))
;    (is "(1 == 2)"
;        (compile "(=* 1 2)"))
;    (is "((1 == 2) && (2 == 3))"
;        (compile "(=* 1 2 3)"))
;    (is "((1 == 2) && (2 == 3) && (3 == 4))"
;        (compile "(=* 1 2 3 4)"))
;    (is "((1 == 2) && (2 == 3) && (3 == 4) && (4 == 5))"
;        (compile "(=* 1 2 3 4 5)"))
;    )
;
;  )
