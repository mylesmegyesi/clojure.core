(ns clojure.lang.lazy-seq
  (:refer-clojure :only [declare defn defn- let list locking loop])
  (:require [clojure.lang.aseq        :refer [seq-equal?]]
            [clojure.lang.deftype     :refer [deftype]]
            [clojure.lang.equivalence :as    equiv]
            [clojure.lang.object      :as    platform-object]
            [clojure.lang.protocols   :refer [ICounted ILazySeq IMeta IObj IPending ISeq ISeqable ISequential
                                              -sval -seq -first -next -more]]
            [clojure.next             :refer :all]))

(declare make-lazy-seq)

(deftype LazySeq [-meta
                  ^:unsynchronized-mutable f
                  ^:unsynchronized-mutable sv
                  ^:unsynchronized-mutable s
                  -seq-lock
                  -sval-lock]

  ICounted
  (-count [this]
    (loop [sq (-seq this)
           cnt 0]
      (if sq
        (recur (-next sq) (inc cnt))
        cnt)))

  ILazySeq
  (-sval [this]
    (locking -sval-lock
      (if (not (nil? f))
        (do
          (set! sv (f))
          (set! f nil)))
      (if (nil? sv)
        s
        sv)))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this new-meta]
    (make-lazy-seq new-meta f sv s))

  IPending
  (-is-realized? [this]
    (nil? f))

  ISequential

  ISeqable
  (-seq [this]
    (locking -seq-lock
      (-sval this)
      (if (not (nil? sv))
        (let [ls (loop [sls sv]
                   (if (instance? LazySeq sls)
                     (recur (-sval sls))
                     (seq sls)))]
          (set! sv nil)
          (set! s ls)))
      s))

  ISeq
  (-first [this]
    (if (nil? s)
      nil
      (-first (-seq this))))

  (-next [this]
    (if (nil? s)
      nil
      (-next (-seq this))))

  (-more [this]
    (if (nil? s)
      (list)
      (-more (-seq this))))

  platform-object/base-object
  (equiv/equals-method [this other]
    (seq-equal? this other)))

(defn make-lazy-seq
  ([-fn]
    (make-lazy-seq nil -fn nil nil))
  ([-meta -fn -sv -seq]
    (LazySeq. -meta -fn -sv -seq (platform-object/new-base-object) (platform-object/new-base-object))))

