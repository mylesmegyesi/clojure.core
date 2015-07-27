(ns clojure.lang.input-output-test
  (:refer-clojure :only [binding fn let reify re-matches])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all])
  (:import  [clojure.lang.platform FallBackNumber]
            [java.util HashMap HashSet LinkedList Stack]))

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
  (testing "pr for Object"
    (is (re-matches
          #"#<Object java.lang.Object.*>"
          (with-out-str (pr (Object.))))))

  (testing "pr for a Number"
    (is (=
          (with-out-str (pr (FallBackNumber. 42)))
          "42")))

  (testing "pr for a Number with print-dup"
    (binding [*print-dup* true]
      (is (=
            (with-out-str (pr (FallBackNumber. 42)))
            "#=(clojure.lang.platform.FallBackNumber. \"42\")"))))

  (testing "pr for a String without print readably or print dup"
    (binding [*print-readably* false]
      (is (=
            (with-out-str (pr "hello world"))
            "hello world"))))

  (testing "pr for a String with print readably or print dup"
    (is (=
          (with-out-str (pr "hello world"))
          "\"hello world\"")))

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

  (testing "pr for a Boolean"
    (is (=
          (with-out-str (pr true))
          "true")))

  (testing "pr for a Boolean with print-dup"
    (is (=
          (with-out-str (pr true))
          "true")))

  (testing "pr for a List"
    (let [ll (LinkedList.)]
      (.add ll 1)
      (.add ll 2)
      (.add ll 3)
      (is (=
            (with-out-str (pr ll))
            "(1 2 3)"))))

  (testing "pr for a RandomAccess"
    (let [stack (Stack.)]
      (.push stack 1)
      (.push stack 2)
      (.push stack 3)
      (is (=
            (with-out-str (pr stack))
            "[1 2 3]"))))

  (testing "pr for a Map"
    (let [hm (HashMap.)]
      (.put hm 1 1)
      (.put hm 2 2)
      (.put hm 3 3)
      (is (=
            (with-out-str (pr hm))
            "{1 1, 2 2, 3 3}"))))

  (testing "pr for a Set"
    (let [hs (HashSet.)]
      (.add hs 1)
      (.add hs 2)
      (.add hs 3)
      (is (=
            (with-out-str (pr hs))
            "#{1 2 3}"))))

  (testing "pr for a Regex"
    (is (=
          (with-out-str (pr #"a p(.*)ttern"))
          "#\"a p(.*)ttern\"")))

  )
