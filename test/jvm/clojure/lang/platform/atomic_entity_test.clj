(ns clojure.lang.platform.atomic-entity-test
  (:refer-clojure :only [and let])
  (:require [clojure.test                        :refer :all]
            [clojure.lang.equivalence            :refer [not =]]
            [clojure.lang.platform.atomic-entity :refer :all]))

(deftest atomic-entity-test
  (testing "the entity is set in the constructor"
    (let [ent (make-atomic-entity "ent")]
      (is (= "ent" (get-entity ent)))))

  (testing "the entity can be set"
    (let [ent (make-atomic-entity "first")]
      (set-entity! ent "second")
      (is (= "second" (get-entity ent)))))

  (testing "the entity can be set if the comparison succeeds"
    (let [ent (make-atomic-entity "first")
          success (compare-and-set-entity! ent "first" "second")]
      (is (and success
               (= "second" (get-entity ent))))))

  (testing "the entity can fail to set if the comparison fails"
    (let [ent (make-atomic-entity "first")
          success (compare-and-set-entity! ent "not first" "second")]
      (is (and (not success)
               (= "first" (get-entity ent))))))
  )
