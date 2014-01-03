(ns clojure.lang.iseq
  (:refer-clojure :only [defprotocol]))

(defprotocol ISeq
  (-first [this])
  (-next [this]))
