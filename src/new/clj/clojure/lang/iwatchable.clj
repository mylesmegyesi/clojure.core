(ns clojure.lang.iwatchable
  (:refer-clojure :only [defprotocol]))

(defprotocol IWatchable
  (-add-watch [this watch-key callback-fn])
  (-remove-watch [this watch-key]))
