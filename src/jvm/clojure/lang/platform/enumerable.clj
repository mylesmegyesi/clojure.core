(ns clojure.lang.platform.enumerable
  (:refer-clojure :only [deftype let reset! defn update-in fn cons list])
  (:require [clojure.lang.seq :refer [first next]]))

(deftype SeqIterator [^:unsynchronized-mutable -current-seq]
  java.util.Iterator

  (hasNext [this]
    (if -current-seq true false))

  (next [this]
    (if -current-seq
      (let [first-item (first -current-seq)]
        (set! -current-seq (next -current-seq))
        first-item)
      (throw (java.util.NoSuchElementException. "What has gone wrong in your life that has led you down this path?"))))

  (remove [this]
    (throw (UnsupportedOperationException. "What're you gonna do?"))))

(defn new-seq-iterator [-seq]
  (SeqIterator. -seq))

(defn platform-enumerable-method [methods]
  (update-in methods
             ['Iterable]
             (fn [old]
               (cons
                 (list 'iterator ['this]
                       (list 'clojure.lang.platform.enumerable/new-seq-iterator
                             (list 'clojure.lang.seqable/seq 'this)))
                 old))))
