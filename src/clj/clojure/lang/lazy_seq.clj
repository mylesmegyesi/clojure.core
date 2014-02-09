(ns clojure.lang.lazy-seq
  (:refer-clojure :only [declare defn defn- deftype instance? let loop])
  (:require [clojure.lang.protocols :refer [IMeta ISeq ISeqable -seq -first -next]]
            [clojure.next           :refer :all]))

(declare make-lazy-seq)

(deftype LazySeq [-meta fn-atm seq-atm]
  IMeta
  (-meta [this] -meta)

  (-with-meta [this new-meta]
    (make-lazy-seq new-meta (deref fn-atm) (deref seq-atm)))

  ISeqable
  (-seq [this]
    (if (deref fn-atm)
      (let [s (loop [sv ((deref fn-atm))]
                (if (instance? LazySeq sv)
                  (recur (-seq sv))
                  (-seq sv)))]
        (reset! seq-atm s)
        (reset! fn-atm nil)
        s)
      (deref seq-atm)))

  ISeq
  (-first [this]
    (let [s (-seq this)]
      (if s
        (-first s)
        nil)))

  (-next [this]
    (let [s (-seq this)]
      (if s
        (-next s)
        nil))))

(defn make-lazy-seq
  ([-fn]
    (LazySeq. nil (atom -fn) (atom nil)))
  ([-meta -fn -seq]
    (LazySeq. -meta (atom -fn) (atom -seq))))
