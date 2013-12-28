(ns clojure.lang.meta
  (:refer-clojure :only [defprotocol]))

(defprotocol Meta
  (meta [this])
  (with-meta [this new-meta]))
