(ns clojure.lang.equivalence
  (:refer-clojure :only [defmacro defprotocol extend-protocol fn defn list list* update-in cons])
  (:require [clojure.lang.protocols :refer [IEquivalence]]))

(extend-protocol IEquivalence
  Object
  (-equivalent? [this other]
    (.equals this other))
  (-equal? [this other]
    (.equals this other)))

(defmacro equals-method [bindings & body]
  `(equals ~bindings ~@body))

