(ns clojure.lang.enumeration-seq
  (:refer-clojure :only [declare defn if-let locking loop satisfies?])
  (:require [clojure.next :refer :all]
            [clojure.lang
              [aseq            :refer [defseq]]
              [deftype]
              [enumerable      :refer [get-next has-more-elements?]]
              [equivalence]
              [object          :refer [new-base-object]]
              [persistent-list :refer [EMPTY-LIST]]
              [protocols       :refer [ICounted IMeta IObj IPersistentCollection
                                       ISeq ISeqable ISequential]]]))

(declare make-enumeration-seq)

(defseq EnumerationSeq [-iter
                        ^:unsynchronized-mutable -val
                        -val-lock
                        ^:unsynchronized-mutable -rest
                        -rest-lock
                        -meta]
  ICounted
  (-count [this]
    (loop [i 1
           sq (next this)]
      (if sq
        (if (and (satisfies? ICounted sq) (not (instance? EnumerationSeq sq)))
          (recur (inc (+ i (count sq))) (next sq))
          (recur (inc i) (next sq)))
        i)))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (make-enumeration-seq -iter -val -rest m))

  IPersistentCollection
  (-cons [this x]
    (cons x this))

  (-empty [this] EMPTY-LIST)

  ISeq
  (-first [this]
    (if (nil? -val)
      (locking -val-lock
        (if (nil? -val)
          (do
            (set! -val (get-next -iter))
            -val)))
    -val))

  (-next [this]
    (if (nil? -rest)
      (locking -rest-lock
        (if (nil? -rest)
          (do
            (first this)
            (set! -rest (make-enumeration-seq -iter))
            -rest)))
      -rest))

  (-more [this]
    (if-let [s (next this)] s EMPTY-LIST)))

(defn make-enumeration-seq
  ([iter]
    (if (has-more-elements? iter)
      (make-enumeration-seq iter nil nil nil)
      nil))
  ([iter v r m] (EnumerationSeq. iter v (new-base-object) r (new-base-object) m)))

