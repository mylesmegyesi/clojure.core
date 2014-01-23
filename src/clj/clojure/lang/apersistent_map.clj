(ns clojure.lang.apersistent-map
  (:refer-clojure :only [bit-and defmacro defn let loop +])
  (:require [clojure.lang.counted   :refer [count]]
            [clojure.lang.hash      :refer [hash]]
            [clojure.lang.lookup    :refer [contains? get]]
            [clojure.lang.map-entry :refer [key val]]
            [clojure.lang.operators :refer [and =]]
            [clojure.lang.seq       :refer [first next seq]]))

(defn map-hash [-seq]
  (loop [entries -seq
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
