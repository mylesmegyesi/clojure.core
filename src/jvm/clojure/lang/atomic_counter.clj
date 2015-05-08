(ns clojure.lang.atomic-counter
  (:refer-clojure :only [defmacro])
  (:import [java.util.concurrent.atomic AtomicInteger]))

(defmacro get-and-increment-atomic-counter [counter]
  `(.getAndIncrement ~counter))

(defmacro new-atomic-counter
  ([] `(AtomicIntger. 1))
  ([value] `(AtomicInteger. ~value)))
