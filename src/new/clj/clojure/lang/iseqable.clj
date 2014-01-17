(ns clojure.lang.iseqable
  (:refer-clojure :only [defprotocol]))

(defprotocol ISeqable
  (-seq [this]))
