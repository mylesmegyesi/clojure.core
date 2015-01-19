(ns clojure.lang.future-test
  (:refer-clojure :only [let while])
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
      (is (realized? fut))))

  (testing "blocking deref can timeout with a default"
    (let [fut (future (sleep 1000))]
      (is (= "default" (deref fut 1 "default")))))

  (testing "a future can be cancelled"
    (let [fut (future (sleep 1000))]
      (future-cancel fut)
      (is (future-cancelled? fut))))

  (testing "a future is a future"
    (is (future? (future "ok")))
    (is (not (future? "ok"))))

  (testing "a future is done"
    (let [finish (atom false)
          fut (future (while (not (deref finish))))]
      (is (not (future-done? fut)))
      (reset! finish true)
      (deref fut)
      (is (future-done? fut))))

  )
