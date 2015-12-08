(ns clojure.lang.future-submission
  (:refer-clojure :only [concat defmacro first list let symbol reify])
  (:require [clojure.next :refer :all :exclude [first]]
            [clojure.lang.agent])
  (:import [java.util.concurrent TimeoutException TimeUnit Future]))

(defmacro get-result
  ([future-submission] `(.get ^Future ~future-submission))
  ([future-submission timeout-ms timeout-val]
    `(try (.get ~future-submission ~timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
      (catch java.util.concurrent.TimeoutException ~(symbol 'e) ~timeout-val))))

(defmacro is-done? [future-submission]
  `(.isDone ^Future ~future-submission))

(defmacro submit-future [f]
  `(let [callable# (reify Callable
                     (call [_] (~f)))]
    (.submit clojure.lang.agent/solo-executor callable#)))

(defmacro cancel [future-submission interrupt]
  `(.cancel ^Future ~future-submission ^Boolean ~interrupt))

(defmacro is-cancelled? [future-submission]
  `(.isCancelled ^Future ~future-submission))

(defmacro is-future? [future-submission]
  `(instance? Future ~future-submission))

(defmacro deffuture [type bindings & body]
  (let [future-submission (first bindings)]
    (concat
      (list 'clojure.core/deftype type bindings
        'java.util.concurrent.Future
        (list 'get ['this] (list '.get ^Future future-submission))
        (list 'get ['this 'timeout-ms 'timeout-val]
          (list '.get ^Future future-submission 'timeout-ms 'timeout-val))
        (list 'isCancelled ['this] (list '.isCancelled ^Future future-submission))
        (list 'isDone ['this] (list '.isDone ^Future future-submission))
        (list 'cancel ['this 'interrupt] (list '.cancel ^Future future-submission 'interrupt)))
      body)))
