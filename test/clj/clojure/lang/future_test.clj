(ns clojure.lang.future-test
  (:refer-clojure :only [let while true?])
  (:require [clojure.test        :refer :all]
            [clojure.next        :refer :all]
            [clojure.lang.thread :refer [sleep]]))

(deftest future-test
  (testing "deref a future"
    (let [fut (future "ok")]
      (is (= "ok" (deref fut)))))

  (testing "a derefable future is realized"
    (let [fut (future "ok")]
      (deref fut)
      (is (true? (realized? fut)))))

  (testing "blocking deref can timeout with a default"
    (let [fut (future (sleep 1000))]
      (is (= "default" (deref fut 1 "default")))))

  )
