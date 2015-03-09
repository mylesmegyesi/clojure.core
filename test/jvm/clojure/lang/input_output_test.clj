(ns clojure.lang.input-output-test
  (:refer-clojure :only [binding let reify true?])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all]))

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

