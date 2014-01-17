(ns clojure.lang.ivalidatable
  (:refer-clojure :only [defprotocol]))

(defprotocol IValidatable
  (-get-validator [this])
  (-set-validator! [this validator-fn]))
