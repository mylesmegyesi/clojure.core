(ns clojure.lang.thread
  (:refer-clojure :only [defmacro defn])
  (:require [clojure.next :refer :all])
  (:import [clojure.lang.platform       Threading]
           [java.util.concurrent        CountDownLatch]
           [java.util.concurrent.atomic AtomicLong]))

(defmacro sleep [millis]
  `(Thread/sleep ~millis))

(defmacro local-state []
  `(ThreadLocal.))

(defmacro get-local-state [state]
  `(.get ~state))

(defmacro set-local-state [state v]
  `(.set ~state ~v))

(defmacro thread-reference []
  `(. Thread currentThread))

(defmacro create-fixed-thread-pool-executor [atomic-long]
  `(. java.util.concurrent.Executors newFixedThreadPool
    (+ 2 (.availableProcessors (. Runtime getRuntime)))
    (. Threading createThreadFactory
      "clojure-agent-send-pool-%d" ~atomic-long)))

(defmacro create-cached-thread-pool-executor [atomic-long]
  `(. java.util.concurrent.Executors newCachedThreadPool
    (. Threading createThreadFactory
      "clojure-agent-send-off-pool-%d" ~atomic-long)))

(defmacro new-countdown-latch [c]
  `(new CountDownLatch ~c))

(defmacro latch-countdown [latch]
  `(. ~latch countDown))

(defmacro latch-await
  ([latch] `(. ~latch await))
  ([latch timeout-ms]
    `(. ~latch await ~timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS))))

