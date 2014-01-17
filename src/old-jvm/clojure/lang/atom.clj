(ns clojure.lang.atom
  (:refer-clojure :only [atom compare-and-set! reset! swap!]
                  :rename {atom core-atom
                           compare-and-set! core-compare-and-set!
                           reset! core-reset!
                           swap! core-swap!}))

(def atom core-atom)
(def compare-and-set! core-compare-and-set!)
(def reset! core-reset!)
(def swap! core-swap!)
