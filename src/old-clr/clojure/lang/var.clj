(ns clojure.lang.var
  (:refer-clojure :only [var? bound?]
                  :rename {var? core-var?
                           bound? core-bound?}))

(def var? core-var?)
(def bound? core-bound?)
