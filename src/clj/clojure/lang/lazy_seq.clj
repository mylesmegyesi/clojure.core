(ns clojure.lang.lazy-seq
  (:refer-clojure :only [declare defn defn- deftype let locking loop])
  (:require [clojure.lang.protocols :refer [ICounted IMeta IObj ISeq ISeqable
                                            -seq -first -next -more]]
            [clojure.next           :refer :all]))

(declare make-lazy-seq)

(deftype LazySeq [-meta fn-atm seq-atm]
  ICounted
  (-count [this]
    (loop [s (-seq this)
           cnt 0]
      (if s
        (recur (-next s) (inc cnt))
        cnt)))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this new-meta]
    (make-lazy-seq new-meta (deref fn-atm) (deref seq-atm)))

  ISeqable
  (-seq [this]
    (locking seq-atm
      (if (deref fn-atm)
        (let [s (loop [sv ((deref fn-atm))]
                  (if (instance? LazySeq sv)
                    (recur (-seq sv))
                    (-seq sv)))]
          (reset! seq-atm s)
          (reset! fn-atm nil)
          s)
        (deref seq-atm))))

  ISeq
  (-first [this]
    (-first (-seq this)))

  (-next [this]
    (-next (-seq this)))

  (-more [this]
    (-more (-seq this))))

(defn make-lazy-seq
  ([-fn]
    (LazySeq. nil (atom -fn) (atom nil)))
  ([-meta -fn -seq]
    (LazySeq. -meta (atom -fn) (atom -seq))))
