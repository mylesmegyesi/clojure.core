(ns clojure.lang.ilookup
  (:refer-clojure :refer [defprotocol]))

(defprotocol ILookup
  (-lookup [this k]))
