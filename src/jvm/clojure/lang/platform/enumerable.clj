(ns clojure.lang.platform.enumerable
  (:refer-clojure :only [deftype let reset! defn update-in fn cons list])
  (:require [clojure.lang.seq :refer [first next]]))

(deftype SeqIterator [-seq-atom]
  java.util.Iterator

  (hasNext [this]
    (if @-seq-atom true false))

  (next [this]
    (if @-seq-atom
      (let [first-item (first @-seq-atom)]
        (reset! -seq-atom (next @-seq-atom))
        first-item)
      (throw (java.util.NoSuchElementException. "What has gone wrong in your life that has led you down this path?"))))

  (remove [this]
    (throw (UnsupportedOperationException. "What're you gonna do?"))))

(defn new-seq-iterator [-seq-atom]
  (SeqIterator. -seq-atom))

(defn platform-enumerable-method [methods]
  (update-in methods
             ['Iterable]
             (fn [old]
               (cons
                 (list 'iterator ['this]
                       (list 'clojure.lang.platform.enumerable/new-seq-iterator
                             (list 'clojure.core/atom
                                   (list 'clojure.lang.seq/seq 'this))))
                 old))))
