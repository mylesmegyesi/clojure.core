(ns clojure.lang.platform.atomic-entity
  (:refer-clojure :only [defn defprotocol deftype let])
  (:require [clojure.lang.operators :refer [=]])
  (:import [System.Threading ReaderWriterLock]))

(defprotocol IAtomicEntity
  (get-entity  [this])
  (set-entity! [this new-value])
  (compare-and-set-entity! [this old-value new-value]))

(def neg-one (Convert/ToInt32 -1))

(deftype ReadWriteAtomicEntity [^:unsynchronized-mutable value lock]
  IAtomicEntity
  (get-entity [this]
    (.AcquireReaderLock lock neg-one)
    (let [v value]
      (.ReleaseReaderLock lock)
      v))

  (set-entity! [this new-value]
    (.AcquireWriterLock lock neg-one)
    (let [old-value value]
      (set! value new-value)
      (.ReleaseWriterLock lock)
      old-value))

  (compare-and-set-entity! [this compare-value new-value]
    (.AcquireWriterLock lock neg-one)
    (let [old-value value
          success? (if (= old-value compare-value)
                     (do (set! value new-value) true)
                     false)]
      (.ReleaseWriterLock lock)
      success?)))

(defn make-atomic-entity [value]
  (ReadWriteAtomicEntity. value (ReaderWriterLock.)))
