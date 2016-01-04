(ns clojure.lang.apersistent-map
  (:refer-clojure :only [cond defn let loop])
  (:require [clojure.lang.exceptions  :refer [new-argument-error]]
            [clojure.lang.key-value   :refer [platform-map-entry-type]]
            [clojure.next             :refer :all]))

(defn map-hash [m]
  (loop [entries (seq m)
         acc     0]
    (if entries
      (let [entry (first entries)]
        (recur
          (next entries)
          (+ acc (bit-and (hash (key entry))
                          (hash (val entry))))))
      acc)))

(defn map-equals? [m1 m2]
  (if (= (count m1) (count m2))
    (loop [m1-seq (seq m1)]
      (if m1-seq
        (let [first-entry (first m1-seq)
              k (key first-entry)
              v (val first-entry)]
          (if (and (contains? m2 k)
                   (= v (get m2 k)))
            (recur (next m1-seq))
            false))
        true))
    false))

(defn map-cons [m o]
  (cond
    (instance? platform-map-entry-type o)
      (assoc m (key o) (val o))
    (vector? o)
      (if (= (count o) 2)
        (assoc m (nth o 0) (nth o 1))
        (throw (new-argument-error "Vector arg to map conj must be a pair")))
    :else
      (loop [mp m
             s (seq o)]
        (if s
          (let [entry (first s)]
            (recur (assoc mp (key entry) (val entry)) (next s)))
          mp))))

