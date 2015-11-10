(ns clojure.lang.array-chunk
  (:refer-clojure :only [declare defn deftype when])
  (:require [clojure.next            :refer :all]
            [clojure.lang
              [exceptions :refer [new-illegal-state-error]]
              [protocols  :refer [IChunk ICounted IIndexed
                                  -nth]]]))

(declare make-array-chunk)

(deftype ArrayChunk [-arr -off -end]
  IChunk
  (-drop-first [this]
    (when (= -off -end)
      (new-illegal-state-error "dropFirst of empty chunk"))
    (make-array-chunk -arr (inc -off) -end))

  ICounted
  (-count [this]
    (- -end -off))

  IIndexed
  (-nth [this i]
    (aget -arr (+ -off i)))

  (-nth [this i not-found]
    (if (and (>= i 0) (< i (count this)))
      (-nth this i)
      not-found)))

(defn make-array-chunk
  ([arr] (ArrayChunk. arr 0 (alength arr)))
  ([arr off] (ArrayChunk. arr off (alength arr)))
  ([arr off end] (ArrayChunk. arr off end)))

