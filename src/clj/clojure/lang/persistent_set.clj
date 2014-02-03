(ns clojure.lang.persistent-set
  (:refer-clojure :only [cons defn every? <= >=])
  (:require [clojure.lang.persistent-hash-set :refer [hash-set]]
            [clojure.lang.protocols           :refer [-conj -difference -disj -intersection -union]]
            [clojure.next                     :refer :all :exclude [every?]]))

(defn conj
  ([this x] (-conj this (cons x nil)))
  ([this x & xs] (-conj this (cons x xs))))

(defn difference
  ([this] this)
  ([this s] (-difference this [s]))
  ([this s & sets] (-difference this (cons s sets))))

(defn disj
  ([this] this)
  ([this x] (-disj this [x]))
  ([this x & xs] (-disj this (cons x xs))))

(defn intersection
  ([this s] (-intersection this [s]))
  ([this s1 s2] (-intersection this [s1 s2]))
  ([this s1 s2 & sets] (-intersection this (cons s1 (cons s2 sets)))))

(defn set [coll]
  (hash-set (seq coll)))

(defn subset? [s1 s2]
  (and (<= (count s1) (count s2))
       (every? #(contains? s2 %) (seq s1))))

(defn superset? [s1 s2]
  (and (>= (count s1) (count s2))
       (every? #(contains? s1 %) (seq s2))))

(defn union
  ([] (hash-set))
  ([this] this)
  ([this s] (-union this [s]))
  ([this s & sets] (-union this (cons s sets))))
