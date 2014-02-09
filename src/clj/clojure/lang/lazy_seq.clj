(ns clojure.lang.lazy-seq
  (:refer-clojure :only [defn defn- deftype instance? let loop])
  (:require [clojure.lang.protocols :refer [ISeq ISeqable -seq -first -next]]
            [clojure.next           :refer :all]))

(deftype LazySeq [fn-atm seq-atm]
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

(defn make-lazy-seq [-fn]
  (LazySeq. (atom -fn) (atom nil)))
