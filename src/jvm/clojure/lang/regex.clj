(ns clojure.lang.regex
  (:refer-clojure :only [defn fn let loop when])
  (:require [clojure.next :refer :all])
  (:import [java.util.regex Matcher Pattern]))

(defn make-pattern [pattern]
  (if (instance? Pattern pattern)
    pattern
    (. Pattern (compile pattern))))

(defn make-matcher [^Pattern pattern s]
  (. pattern (matcher s)))

(defn get-groups [^Matcher matcher]
  (let [gc (. matcher groupCount)]
    (if (zero? gc)
      (. matcher group)
      (loop [ret (vector)
             c 0]
        (if (<= c gc)
          (recur (conj ret (. matcher group c)) (inc c))
          ret)))))

(defn get-matches [^Pattern pattern s]
  (let [m (make-matcher pattern s)]
    (when (. m matches)
      (get-groups m))))

(defn regex-find [^Matcher matcher]
  (when (. matcher find)
    (get-groups matcher)))

(defn regex-seq [pattern s]
  (let [m (make-matcher pattern s)]
    ((fn step []
       (when (. m find)
         (cons (get-groups m) (lazy-seq (step))))))))

