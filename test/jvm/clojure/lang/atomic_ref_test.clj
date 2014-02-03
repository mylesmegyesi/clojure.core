(ns clojure.lang.atomic-ref-test
  (:refer-clojure :only [let])
  (:require [clojure.test            :refer :all]
            [clojure.lang.atomic-ref :refer [new-atomic-ref ref-get
                                             ref-set! ref-compare-and-set!]]
            [clojure.next            :refer :all]))

(deftest atomic-ref-test
  (testing "the ref is set in the constructor"
    (let [ref (new-atomic-ref "ent")]
      (is (= "ent" (ref-get ref)))))

  (testing "the entity can be set"
    (let [ent (new-atomic-ref "first")]
      (ref-set! ent "second")
      (is (= "second" (ref-get ent)))))

  (testing "the entity can be set if the comparison succeeds"
    (let [ref (new-atomic-ref "first")
          success (ref-compare-and-set! ref "first" "second")]
      (is (and success
               (= "second" (ref-get ref))))))

  (testing "the entity can fail to set if the comparison fails"
    (let [ref (new-atomic-ref "first")
          success (ref-compare-and-set! ref "not first" "second")]
      (is (and (not success)
               (= "first" (ref-get ref)))))))
