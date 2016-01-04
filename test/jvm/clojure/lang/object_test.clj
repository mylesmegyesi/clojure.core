(ns clojure.lang.object-test
  (:refer-clojure :only [])
  (:require [clojure.test                         :refer :all]
            [clojure.next                         :refer :all]
            [clojure.support.exception-assertions :refer [class-cast-exception-is-thrown?]]))

(deftest cast-test
  (testing "casting can throw a class cast exception"
    (class-cast-exception-is-thrown? #"Cannot cast java.lang.Object to java.lang.Number" (cast Number (Object.))))

  (testing "casting from one type to another"
    (is (number? (cast Number 1)))))

