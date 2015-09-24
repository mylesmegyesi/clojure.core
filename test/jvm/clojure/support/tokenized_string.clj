(ns clojure.support.tokenized-string
  (:refer-clojure :only [defn])
  (:import [java.util StringTokenizer]))

(defn tokenize [^String s]
  (StringTokenizer. s))

