(ns clojure.lang.enumeration-seq-test
  (:refer-clojure :only [let])
  (:require [clojure.next                     :refer :all]
            [clojure.test                     :refer :all]
            [clojure.lang.persistent-list     :refer [EMPTY-LIST]]
            [clojure.support.tokenized-string :refer :all]))

(deftest enumertion-seq-test
  (testing "enumeration-seq of an empty enumerable is nil"
    (let [eseq (enumeration-seq (tokenize ""))]
      (is (nil? eseq))))

  (testing "enumeration-seq can hold a meta value"
    (let [eseq (enumeration-seq (tokenize "oh hello world"))
          eseq-with-meta (with-meta eseq {:so :meta})]
      (is (= {:so :meta} (meta eseq-with-meta)))))

  (testing "cons an element"
    (let [eseq (cons (enumeration-seq (tokenize "oh hello world")) "!")]
      (is (= 2 (count eseq)))
      (is (= (enumeration-seq (tokenize "oh hello world")) (first eseq)))
      (is (= \! (second eseq)))))

  (testing "empty returns the empty list"
    (let [eseq (enumeration-seq (tokenize "oh hello world"))]
      (is (= EMPTY-LIST (empty eseq)))))

  (testing "return the first of the seq in a repeatable way"
    (let [eseq (enumeration-seq (tokenize "oh hello world"))]
      (is (= "oh" (first eseq)))
      (is (= "oh" (first eseq)))))

  (testing "return the next of the seq in a repeatable way"
    (let [eseq (enumeration-seq (tokenize "oh hello world"))]
      (is (= "hello" (first (next eseq))))
      (is (= "hello" (first (next eseq))))
      (is (= "world" (first (next (next eseq)))))
      (is (= "world" (first (next (next eseq)))))))

  (testing "next returns nil when there are no more elements"
    (let [eseq (enumeration-seq (tokenize "!"))]
      (is (nil? (next eseq)))))

  (testing "rest returns the empty list when there are no more elements"
    (let [eseq (enumeration-seq (tokenize "!"))]
      (is (= EMPTY-LIST (rest eseq))))))

