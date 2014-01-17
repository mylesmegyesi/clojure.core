(ns clojure.lang.ideref
  (:refer-clojure :only [defprotocol]))

(defprotocol IDeref
  (-deref [this]))
