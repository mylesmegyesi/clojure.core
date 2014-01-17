(ns clojure.lang.ishow
  (:refer-clojure :only [defprotocol]))

(defprotocol IShow
  (-show [this]))
