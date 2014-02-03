(ns clojure.lang.platform.equivalence
  (:refer-clojure :only [defmacro defprotocol extend-protocol fn defn list update-in cons])
  (:require [clojure.lang.protocols :refer [IEquivalence]]))

(extend-protocol IEquivalence
  Object
  (-equivalent? [this other]
    (.equals this other))
  (-equal? [this other]
    (.equals this other)))

(defn platform-equals-method [methods init-macro]
  (update-in methods
             ['Object]
             (fn [old]
               (cons
                 (list 'equals ['this 'other]
                       (list init-macro 'this 'other))
                 old))))
