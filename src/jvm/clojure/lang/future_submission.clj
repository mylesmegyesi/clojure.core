(ns clojure.lang.future-submission
  (:refer-clojure :only [assoc concat defmacro first list let symbol vary-meta])
  (:require [clojure.next :refer :all :exclude [first vary-meta assoc]]
            [clojure.lang.agent])
  (:import [java.util.concurrent TimeoutException TimeUnit Future]
           [clojure.lang.platform FutureSubmission]))

(defmacro get-result
  ([future-submission] `(.get ~future-submission))
  ([future-submission timeout-ms timeout-val]
    `(try (.get ~future-submission ~timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
      (catch java.util.concurrent.TimeoutException ~(symbol 'e) ~timeout-val))))

(defmacro is-done? [future-submission]
  `(.isDone ~future-submission))

(defmacro submit-future [f]
  `(FutureSubmission/submitFutureForceCallable clojure.lang.agent/solo-executor ~f))

(defmacro cancel [future-submission interrupt]
  `(.cancel ~future-submission ~interrupt))

(defmacro is-cancelled? [future-submission]
  `(.isCancelled ~future-submission))

(defmacro is-future? [future-submission]
  `(instance? java.util.concurrent.Future ~future-submission))

(defmacro deffuture [type bindings & body]
  (let [future-submission (first bindings)]
    (concat
      (list 'clojure.core/deftype type bindings
        'java.util.concurrent.Future
        (list 'get ['this] (list '.get future-submission))
        (list 'get ['this 'timeout-ms 'timeout-val]
          (list '.get future-submission 'timeout-ms 'timeout-val))
        (list 'isCancelled ['this] (list '.isCancelled future-submission))
        (list 'isDone ['this] (list '.isDone future-submission))
        (list 'cancel ['this 'interrupt] (list '.cancel future-submission 'interrupt)))
      body)))
