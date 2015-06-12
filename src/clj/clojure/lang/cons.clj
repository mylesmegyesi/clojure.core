(ns clojure.lang.cons
  (:refer-clojure :only [declare defn deftype list nil?])
  (:require [clojure.next           :refer :all]
            [clojure.lang.aseq      :refer [defseq]]
            [clojure.lang.protocols :refer [ICounted IMeta IObj ISeq]]))

(declare make-cons)

(defseq Cons [-meta -first -more]
  ICounted
  (-count [this]
    (inc (count -more)))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this mta]
    (make-cons mta -first -more))

  ISeq
  (-first [this] -first)

  (-next [this] (seq -more))

  (-more [this]
    (if (nil? -more)
      (list)
      -more)))

(defn make-cons
  ([elem s]
    (Cons. {} elem s))
  ([mta elem s]
    (Cons. mta elem s)))
