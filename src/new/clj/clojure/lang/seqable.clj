(ns clojure.lang.seqable
  (:refer-clojure :only [defn])
  (:require [clojure.lang.iseqable :refer [-seq]]))

(defn seq [i]
  (-seq i))
