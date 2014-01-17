(ns clojure.lang.symbol
  (:refer-clojure :only [symbol symbol?]
                  :rename {symbol core-symbol
                           symbol? core-symbol?}))

(def symbol core-symbol)
(def symbol? core-symbol?)
