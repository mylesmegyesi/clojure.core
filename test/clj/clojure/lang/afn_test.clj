(ns clojure.lang.afn-test
  (:refer-clojure :only [let reify])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all]
            [clojure.lang
              [afn             :refer :all]
              [persistent-list :refer [list]]
              [protocols       :refer :all]]
            [clojure.support.exception-assertions :refer [arity-exception-is-thrown?]]))

(deffn TestFn []
  IFn
  (-invoke [this] "arity0")
  (-invoke [this _ _] "arity2")
  (-invoke [this _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ v] v))

(deftest afn-test
  (testing "can invoke defined arities"
    (is (= "arity0" (-invoke (TestFn.))))
    (is (= "arity2" (-invoke (TestFn.) :foo :bar))))

  (testing "will raise arity error if the wrong arity is passed"
    (arity-exception-is-thrown? #".*\(1\) passed to: TestFn.*" (-invoke (TestFn.) :foo)))

  (testing "can apply-to with a seq of args"
    (is (= "arity2" (-apply-to (TestFn.) (list :foo :bar)))))

  (testing "all args after the 18th place will be applied as a vector"
    (is (= (vector "arg19" "arg20" "arg21")
           (-apply-to (TestFn.) (list "arg1" "arg2" "arg3" "arg4" "arg5" "arg6" "arg7" "arg8" "arg9" "arg10" "arg11" "arg12" "arg13" "arg14" "arg15" "arg16" "arg17" "arg18" "arg19" "arg20" "arg21"))))))

(deftest partial-test
  (testing "no arguments returns the fn"
    (let [f (TestFn.)]
      (is (= f (partial f)))))

  (testing "partially apply an argument"
    (let [f0 (reify IFn
               (-invoke [this a b] (+ a b)))
          f (partial f0 1)]
      (is (= 3 (-invoke f 2)))))

  (testing "partially apply many arguments"
    (let [f0 (reify IFn
               (-invoke [this a b c d e f g] (+ a b c d e f g)))
          f (partial f0 1 2 3 4)]
      (is (= 28 (-invoke f 5 6 7)))))

  (testing "apply over max arguments"
    (let [f (partial (TestFn.) "arg1" "arg2" "arg3" "arg4" "arg5" "arg6" "arg7" "arg8" "arg9" "arg10" "arg11" "arg12" "arg13" "arg14" "arg15" "arg16" "arg17" "arg18" "arg19" "arg20")]
      (is (= (vector "arg19" "arg20" "arg21") (-invoke f "arg21"))))
    (let [f (partial (TestFn.))]
      (is (= (vector "arg19" "arg20")
             (-apply-to f (vector "arg1" "arg2" "arg3" "arg4" "arg5" "arg6" "arg7" "arg8" "arg9" "arg10" "arg11" "arg12" "arg13" "arg14" "arg15" "arg16" "arg17" "arg18" "arg19" "arg20")))))))

