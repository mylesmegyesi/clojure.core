(ns clojure.lang.persistent-struct-map
  (:refer-clojure :only [apply declare defn defn- deftype interleave let loop range when])
  (:require [clojure.lang.apersistent-map     :refer [defmap map-cons]]
            [clojure.lang.aseq                :refer [defseq]]
            [clojure.lang.exceptions          :refer [new-argument-error
                                                      new-runtime-exception]]
            [clojure.lang.map-entry           :refer [new-map-entry]]
            [clojure.lang.object              :refer [new-base-object]]
            [clojure.lang.persistent-hash-map :refer [EMPTY-HASH-MAP]]
            [clojure.lang.persistent-list     :refer [EMPTY-LIST]]
            [clojure.lang.protocols           :refer [-get-keys -get-keyslots
                                                      IAssociative ICounted IDef ILookup
                                                      IMeta IObj IPersistentCollection
                                                      IPersistentMap ISeq ISeqable]]
            [clojure.next                     :refer :all]))

(deftype Def [-keys -keyslots]
  IDef
  (-get-keys [this] -keys)
  (-get-keyslots [this] -keyslots))

(defn make-def [ks]
  (when (nil? ks)
    (throw (new-argument-error "Must supply keys")))
  (let [sq (interleave ks (range 1 (inc (count ks))))]
    (Def. ks (apply array-map sq))))

(declare make-struct-map)
(declare make-struct-map-seq)

(defseq PersistentStructMapSeq [-i -keys -vals -ext -meta]
  ICounted
  (-count [this]
    (loop [s (next this)
           i 1]
      (if (nil? s)
        i
        (recur (next s) (inc i)))))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (if (= m -meta)
      this
      (make-struct-map-seq -i -keys -vals -ext m)))

  ISeq
  (-first [this]
    (new-map-entry (first -keys) (aget -vals -i)))

  (-next [this]
    (if (< (inc -i) (alength -vals))
      (make-struct-map-seq (inc -i) (next -keys) -vals -ext -meta)
      (seq -ext)))

  (-more [this]
    (let [sq (next this)]
      (if sq sq EMPTY-LIST))))

(defn- make-struct-map-seq [-i -keys -vals -ext -meta]
  (PersistentStructMapSeq. -i -keys -vals -ext -meta))

(defmap PersistentStructMap [-def -vals -ext -meta]
  IAssociative
  (-assoc [this k v]
    (let [sentinel (new-base-object)
          ve (get (-get-keyslots -def) k sentinel)]
      (if (not (identical? ve sentinel))
        (let [new-vals (aclone -vals)]
          (aset new-vals (dec ve) v)
          (make-struct-map -def new-vals -ext -meta))
        (make-struct-map -def -vals (assoc -ext k v) -meta))))

  (-contains-key? [this k]
    (or (contains? (-get-keyslots -def) k)
        (contains? -ext k)))

  ICounted
  (-count [this]
    (+ (alength -vals) (count -ext)))

  ILookup
  (-lookup [this k not-found]
    (let [sentinel (new-base-object)
          ve (get (-get-keyslots -def) k sentinel)]
      (if (not (identical? ve sentinel))
        (aget -vals (dec ve))
        (if (contains? -ext k)
          (get -ext k)
          not-found))))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (if (= -meta m)
      this
      (make-struct-map -def -vals -ext m)))

  IPersistentCollection
  (-cons [this o]
    (map-cons this o))

  (-empty [this]
    (make-struct-map -def (object-array (count (-get-keyslots -def))) EMPTY-HASH-MAP nil))

  IPersistentMap
  (-dissoc [this k]
    (let [struct-entry (get (-get-keyslots -def) k)]
      (if struct-entry
        (let [new-ext (dissoc -ext k)]
          (if (= -ext new-ext)
            this
            (make-struct-map -def -vals new-ext -meta)))
        (throw (new-runtime-exception "Can't remove struct key")))))

  ISeqable
  (-seq [this]
    (make-struct-map-seq 0 (-get-keys -def) -vals -ext nil)))

(defn make-struct-map [-def -vals -ext -meta]
  (PersistentStructMap. -def -vals -ext -meta))

