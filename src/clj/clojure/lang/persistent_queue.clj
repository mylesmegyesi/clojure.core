(ns clojure.lang.persistent-queue
  (:refer-clojure :only [declare defn if-let let list loop satisfies?])
  (:require [clojure.next :refer :all]
            [clojure.lang
              [aseq      :refer [defseq]]
              [deftype   :refer [deftype]]
              [equivalence]
              [protocols :refer [ICounted IPersistentStack IPersistentCollection IPersistentQueue
                                 IMeta IObj
                                 ISeq ISeqable ISequential]]]))

(declare make-queue)

(defseq PersistentQueueSeq [-meta -seq -vec-seq]
  ICounted
  (-count [this]
    ; Hack for a mixture of types, -seq can either be a clojure PersistentList,
    ; a clojure.core PersistentVector, or nil.
    ; Once clojure.core owns PersistentList then clojure.core/count will suffice.
    (if (satisfies? ICounted -seq)
      (+ (count -seq) (count -vec-seq))
      (+ (clojure.core/count -seq) (count -vec-seq))))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m] (PersistentQueueSeq. m -seq -vec-seq))

  ISeq
  (-first [this] (first -seq))

  (-next [this]
    (if-let [next-seq (next -seq)]
      (PersistentQueueSeq. nil next-seq -vec-seq)
      (if (nil? -vec-seq)
        nil
        (PersistentQueueSeq. nil -vec-seq nil))))

  (-more [this] (seq (next this)))

  )

(deftype PersistentQueue [-meta -length -seq -vec]
  ICounted
  (-count [this] -length)

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m] (make-queue m -length -seq -vec))

  IPersistentCollection
  (-cons [this x]
    (if (nil? -seq)
      (make-queue -meta (inc -length) (list x) nil)
      (let [-new-vec (if (nil? -vec)
                       (conj (vector) x)
                       (conj -vec x))]
        (make-queue -meta (inc -length) -seq -new-vec))))

  (-empty [this] (make-queue -meta 0 nil nil))

  IPersistentStack
  (-peek [this] (first -seq))

  (-pop [this]
    (if (nil? -seq)
      this
      (if-let [new-seq (next -seq)]
        (make-queue -meta (dec -length) new-seq -vec)
        (let [new-seq (seq -vec)]
          (make-queue -meta (dec -length) new-seq nil)))))

  IPersistentQueue
  (-contains [this k]
    (loop [s (seq this)]
      (if (nil? s)
        false
        (if (= k (first s))
          true
          (recur (next s))))))

  ISeqable
  (-seq [this]
    (if (nil? -seq)
      nil
      (PersistentQueueSeq. nil -seq (seq -vec))))

  )

(defn make-queue [-meta -length -seq -vec]
  (PersistentQueue. -meta -length -seq -vec))

(def EMPTY-QUEUE (make-queue nil 0 nil nil))

