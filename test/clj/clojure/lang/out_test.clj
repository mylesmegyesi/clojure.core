(ns clojure.lang.out-test
  (:refer-clojure :only [binding let nil? reify re-matches subs])
  (:require [clojure.test                       :refer :all]
            [clojure.next                       :refer :all]
            [clojure.support.test-seq           :refer [test-seq]]
            [clojure.lang.protocols             :refer [IMeta ISeq ISeqable ISequential]]))

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

  (testing "pr for a map"
    (is (=
          (with-out-str (pr (array-map :hello :world)))
          "{:hello :world}")))

  (testing "pr for a vector"
    (is (=
          (with-out-str (pr (vector 1 2 3)))
          "[1 2 3]")))

  (testing "pr for a set"
    (is (=
          (with-out-str (pr (sorted-set 1 2 3)))
          "#{1 2 3}")))

  (testing "pr for a seq"
    (is (=
          (with-out-str (pr (test-seq '(1 2 3))))
          "(1 2 3)")))

  (testing "pr for a seq with meta"
    (binding [*print-meta* true
              *print-readably* true]
      (let [seq-with-meta (reify
                            ISequential
                            ISeq
                            (-first [this] 1)
                            (-next [this] nil)
                            ISeqable
                            (-seq [this] this)
                            IMeta
                            (-meta [this] (array-map :so :meta)))]
        (is (= "^{:so :meta} (1)"
              (with-out-str (pr seq-with-meta)))))))

  (testing "pr sequential for a zero print-level"
    (binding [*print-level* 0]
      (is (= "#"
             (with-out-str (pr (test-seq '(1 2 3))))))))

  (testing "pr sequential for a zero print-length"
    (binding [*print-length* 0]
      (is (= "(...)"
              (with-out-str (pr (test-seq '(1 2 3))))))))

  (testing "pr sequential for a print-length less than seq size"
    (binding [*print-length* 2]
      (is (= "(1 2 ...)"
             (with-out-str (pr (test-seq '(1 2 3)))))))))

(deftest newline-test
  (testing "newline returns nil"
    (with-out-str
      (is (nil? (newline))))))

(deftest flush-test
  (testing "flush returns nil"
    (with-out-str
      (is (nil? (flush))))))

