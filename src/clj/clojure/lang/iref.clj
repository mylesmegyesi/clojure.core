(ns clojure.lang.iref
  (:refer-clojure :only [defprotocol]))

(defprotocol IRef
  (-get-validator [this])
  (-set-validator! [this validator-fn])
  (-add-watch [this watch-key callback-fn])
  (-remove-watch [this watch-key]))
