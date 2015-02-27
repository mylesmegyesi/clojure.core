(ns clojure.lang.out-test
  (:refer-clojure :only [binding let nil? reify re-matches subs])
  (:require [clojure.test             :refer :all]
            [clojure.next             :refer :all]
            [clojure.support.test-seq :refer [test-seqable]]
            [clojure.lang.protocols   :refer [IMeta ISeqable]]))

(deftest print-simple-test
  (testing "writes the to string version of an obj"
    (let [o (reify
              IMeta
              (-meta [_] nil))]
      (is (=
            (with-out-str (print-simple o *out*))
            (str o))))))

(deftest pr-test
  (testing "pr for a :default meta type that is not an IObj"
    (let [obj (reify
                IMeta
                (-meta [_] (array-map :type :default)))]
      (is (=
            (with-out-str (pr obj))
            (str obj)))))

  (testing "pr for nil is the string 'nil'"
    (is (=
          (with-out-str (pr nil))
          "nil")))

  (testing "pr for a keyword"
    (is (=
          (with-out-str (pr (keyword "test")))
          ":test")))

  (testing "pr for a symbol"
    (is (=
          (with-out-str (pr (symbol "test")))
          "test")))

  (testing "pr for a seq"
    (is (re-matches
          #".*TestSeqable.*"
          (with-out-str (pr (test-seqable '(1 2 3)))))))

  (testing "pr for a seq with meta"
    (binding [*print-meta* true
              *print-readably* true]
      (let [seq-with-meta (reify
                            ISeqable
                            (-seq [this] '(1 2 3))
                            IMeta
                            (-meta [this] (array-map :so :meta)))]
        (is (= "^{:so :meta}"
              (subs (with-out-str (pr seq-with-meta)) 0 12))))))

  (testing "pr for a map"
    (is (=
          (with-out-str (pr (array-map :hello :world)))
          "{:hello :world}")))

  )

(deftest newline-test
  (testing "newline returns nil"
    (with-out-str
      (is (nil? (newline))))))

(deftest flush-test
  (testing "flush returns nil"
    (with-out-str
      (is (nil? (flush))))))

