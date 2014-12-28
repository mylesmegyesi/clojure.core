(ns clojure.lang.enumerable
  (:refer-clojure :only [deftype let reset! defn update-in fn cons list])
  (:require [clojure.next :refer [first next] :exclude [cons]]))

(deftype SeqIterator [^:unsynchronized-mutable -current-seq]
  java.util.Iterator

  (hasNext [this]
    (if -current-seq true false))

  (next [this]
    (if -current-seq
      (let [first-item (first -current-seq)]
        (set! -current-seq (next -current-seq))
        first-item)
      (throw (java.util.NoSuchElementException. ""))))

  (remove [this]
    (throw (UnsupportedOperationException. ""))))

(defn new-seq-iterator [-seq]
  (SeqIterator. -seq))

(defn platform-enumerable-method [methods]
  (update-in methods
             ['Iterable]
             (fn [old]
               (cons
                 (list 'iterator ['this]
                       (list 'clojure.lang.enumerable/new-seq-iterator
                             (list 'clojure.next/seq 'this)))
                 old))))
