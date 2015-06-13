(ns clojure.lang.platform.future-test
  (:refer-clojure :only [let])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all]))

(deftest java-util-concurrent-future-test
  (testing "can cancel a future"
    (let [fut (future (Thread/sleep 1000))]
      (.cancel fut true)
      (is (true? (.isCancelled fut)))))

  (testing "query isDone"
    (let [fut (future "ok")]
      (while (not (realized? fut)))
      (is (true? (.isDone fut)))))

  (testing "get a future result"
    (let [fut (future "ok")]
      (is (= "ok" (.get fut)))))

  (testing "throws an exception is a timeout occurs with get"
    (let [fut (future (Thread/sleep 1000))]
      (try
        (do
          (.get fut 1 java.util.concurrent.TimeUnit/MILLISECONDS)
          (is false))
        (catch java.util.concurrent.TimeoutException _
          (is true)))))

  )
