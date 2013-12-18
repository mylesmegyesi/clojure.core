(ns clojure.ruby.main-test
  (:require [clojure.test      :refer :all]
            [clojure.ruby.main :refer :all]))

(deftest parse-args-test
  (testing "parses one load path"
    (is (= ["one"]
           (:load-path (parse-args ["-I" "one"])))))

  (testing "parses two load paths"
    (is (= ["one" "two"]
           (:load-path (parse-args ["-I" "one" "-I" "two"])))))

  (testing "ignores -I when no value is given"
    (is (= ["one"]
           (:load-path (parse-args ["-I" "one" "-I"])))))

  (testing "parses one input directory"
    (is (= ["one"]
           (:input-directories (parse-args ["one"])))))

  (testing "parses two input filenames"
    (is (= ["one"]
           (:input-directories (parse-args ["one"])))))

  (testing "parses output directory"
    (is (= "some-file"
           (:output-directory (parse-args ["-o" "some-file"])))))

  (testing "ignores -o when no value is given"
    (is (= nil
           (:output-directory (parse-args ["-o"])))))

  (testing "all together"
    (is (= {:load-path         ["one" "two"]
            :input-directories ["three" "four"]
            :output-directory  "out"}
           (parse-args ["-I" "one" "-I" "two" "-o" "out" "three" "four"]))))
  )
