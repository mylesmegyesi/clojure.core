(ns clojure.lang.input-output-test
  (:refer-clojure :only [binding class fn let reify])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all])
  (:import  [clojure.lang.platform FallBackNumber]))

(deftest platform-print-constructor-test
  (testing "printing the constructor without print-args"
    (is (=
          (with-out-str (print-ctor (Object.) (fn [_ _]) *out*))
          "#=(java.lang.Object. )")))

  (testing "printing the constructor with print-args"
    (is (=
          (with-out-str (print-ctor (Object.) (fn [o w] (.write w (str (.isArray (class o)) " hello world")) ) *out*))
          "#=(java.lang.Object. false hello world)"))))

(deftest default-out-test
  (testing "*out* is an OutputStreamWriter by default"
    (is (instance? java.io.OutputStreamWriter *out*))))

(deftest newline-test
  (testing "newline uses the system line.separator property"
    (is (=
          (with-out-str (newline))
          (System/getProperty "line.separator")))))

(deftest flush-test
  (testing "flush is invoked on *out*"
    (let [flushed (atom false)
          o (reify
              java.io.Flushable
              (flush [_] (reset! flushed true)))]
      (binding [*out* o]
        (flush)
        (is (true? (deref flushed)))))))

(deftest platform-pr-test
  (testing "pr for a Number"
    (is (=
          (with-out-str (pr (FallBackNumber. 42)))
          "42")))

  (testing "pr for a BigDecimal"
    (is (=
          (with-out-str (pr 42.2M))
          "42.2M")))

  (testing "pr for a BigInt"
    (is (=
          (with-out-str (pr 42N))
          "42N")))

  (testing "pr for a Class"
    (is (=
          (with-out-str (pr (class (Object.))))
          "java.lang.Object")))

  )
