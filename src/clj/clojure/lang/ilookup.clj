(ns clojure.lang.ilookup
  (:refer-clojure :refer [defprotocol]))

(defprotocol ILookup
  (-includes? [this k])
  (-lookup [this k not-found]))
