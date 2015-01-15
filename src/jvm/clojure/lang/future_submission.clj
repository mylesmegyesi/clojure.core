(ns clojure.lang.future-submission
  (:refer-clojure :only [concat defmacro first list let symbol])
  (:require [clojure.next :refer :all :exclude [first]]
            [clojure.lang.agent])
  (:import [java.util.concurrent TimeoutException TimeUnit Future]))

(defmacro get-result
  ([future-submission] `(.get ~future-submission))
  ([future-submission timeout-ms timeout-val]
    `(try (.get ~future-submission ~timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
      (catch java.util.concurrent.TimeoutException ~(symbol 'e) ~timeout-val))))

(defmacro is-done? [future-submission]
  `(.isDone ~future-submission))

(defmacro submit-future [f]
  `(.submit clojure.lang.agent/solo-executor ^Callable ~f))

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
