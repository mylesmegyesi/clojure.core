(ns clojure.lang.chunk-buffer
  (:refer-clojure :only [defn defprotocol deftype let])
  (:require [clojure.next             :refer :all]
            [clojure.lang.array-chunk :refer [make-array-chunk]]
            [clojure.lang.protocols   :refer [ICounted]]))

(defprotocol ^:private IChunkBuffer
  (-add [this o])
  (-chunk [this]))

(deftype ChunkBuffer [^:unsynchronized-mutable -buffer
                      ^:unsynchronized-mutable -end]

  ICounted
  (-count [this] -end)

  IChunkBuffer
  (-add [this o]
    (aset -buffer -end o)
    (set! -end (inc -end)))

  (-chunk [this]
    (let [ret (make-array-chunk -buffer 0 -end)]
      (set! -buffer nil)
      ret)))

(defn make-chunk-buffer [capacity]
  (ChunkBuffer. (object-array capacity) 0))

