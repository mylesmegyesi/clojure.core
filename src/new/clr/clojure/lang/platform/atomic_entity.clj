(ns clojure.lang.platform.atomic-entity
  (:refer-clojure :only [defn defprotocol reify let])
  (:import [System.Threading Interlocked]))

(defprotocol IAtomicEntity
  (get-entity  [this])
  (set-entity! [this new-value])
  (compare-and-set-entity! [this old-value new-value]))

(defn make-atomic-entity [value]
  (reify IAtomicEntity
    (get-entity [this] value)

    (set-entity! [this new-value]
      (Interlocked/Exchange (by-ref value) new-value))

    (compare-and-set-entity! [this old-value new-value]
      (Object/ReferenceEquals
        old-value
        (Interlocked/CompareExchange (by-ref value) new-value old-value)))))
