(ns clojure.lang.seq
  (:refer-clojure :only [cond defn let nil?])
  (:require [clojure.lang.iseq      :refer [-first -next]]
            [clojure.lang.operators :refer [not]]
            [clojure.lang.seqable   :refer [seq]]))

(defn first [s]
  (-first s))

(defn next [s]
  (-next s))

(defn every? [pred seqable]
  (let [s (seq seqable)]
    (cond
      (nil? s) true
      (pred (first s)) (recur pred (next s))
      :else false)))

(defn empty? [seqable]
  (not (seq seqable)))
