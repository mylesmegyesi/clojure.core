(ns clojure.lang.inamed
  (:refer-clojure :only [defprotocol]))

(defprotocol INamed
  (-name [this])
  (-namespace [this]))
