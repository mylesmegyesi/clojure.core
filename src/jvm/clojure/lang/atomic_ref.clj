(ns clojure.lang.atomic-ref
  (:refer-clojure :only [defmacro])
  (:import [java.util.concurrent.atomic AtomicReference]))

(defmacro ref-get [ref]
  `(.get ~ref))

(defmacro ref-set! [ref new-value]
  `(.set ~ref ~new-value))

(defmacro ref-compare-and-set! [ref old-value new-value]
  `(.compareAndSet ~ref ~old-value ~new-value))

(defmacro new-atomic-ref
  ([]
   `(AtomicReference.))
  ([value]
   `(AtomicReference. ~value)))
