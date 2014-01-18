(ns clojure.lang.persistent-set
  (:refer-clojure :only [conj disj set]
                  :rename {conj core-conj
                           disj core-disj
                           set core-set})
  (:require [clojure.set :refer [difference intersection subset? superset? union] :rename {difference set-difference
                                                                                           intersection set-intersection
                                                                                           subset? set-subset?
                                                                                           superset? set-superset?
                                                                                           union set-union}]))

(def conj core-conj)
(def disj core-disj)
(def set  core-set)

(def difference   set-difference)
(def intersection set-intersection)
(def subset?      set-subset?)
(def superset?    set-superset?)
(def union        set-union)
