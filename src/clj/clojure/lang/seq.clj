(ns clojure.lang.seq
  (:refer-clojure :only [defn])
  (:require [clojure.lang.iseq     :refer [-first -next]]
            [clojure.lang.iseqable :refer [-seq]]))

(defn first [s]
  (-first s))

(defn next [s]
  (-next s))

(defn seq [i]
  (-seq i))
