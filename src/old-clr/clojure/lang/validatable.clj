(ns clojure.lang.validatable
  (:refer-clojure :only [get-validator set-validator]
                  :rename {get-validator core-get-validator
                           set-validator core-set-validator}))

(def get-validator core-get-validator)
(def set-validator! core-set-validator)
