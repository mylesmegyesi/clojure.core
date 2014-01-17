(ns clojure.lang.icounted
  (:refer-clojure :only [defprotocol]))

(defprotocol ICounted
  (-count [this]))
