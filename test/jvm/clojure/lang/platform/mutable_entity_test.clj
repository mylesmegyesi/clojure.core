(ns clojure.lang.platform.mutable-entity-test
  (:refer-clojure :only [let =])
  (:require [clojure.test                         :refer :all]
            [clojure.lang.platform.mutable-entity :refer :all]))

(deftest mutable-entity-test
  (testing "the entity is set in the constructor"
    (let [ent (make-mutable-entity "ent")]
      (is (= "ent" (get-entity ent)))))

  (testing "the entity can be set"
    (let [ent (make-mutable-entity "first")]
      (set-entity! ent "second")
      (is (= "second" (get-entity ent)))))
  )
