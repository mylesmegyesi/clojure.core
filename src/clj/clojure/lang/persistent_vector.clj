(ns clojure.lang.persistent-vector
  (:refer-clojure :only [cond declare defn- defn defprotocol deftype let loop nil? when < > >= ->])
  (:require [clojure.next            :refer :all :exclude [bit-shift-left unsigned-bit-shift-right]]
            [clojure.lang.exceptions :refer [new-argument-error new-out-of-bounds-exception
                                             new-illegal-access-error new-illegal-state-error]]
            [clojure.lang.numbers    :refer [->int]]
            [clojure.lang.hash-map   :refer [->bitnum bit-shift-left unsigned-bit-shift-right]]
            [clojure.lang.protocols  :refer [-as-transient -assoc-n -conj! -persistent
                                             IAssociative ICounted IEditableCollection IMeta IObj
                                             IPersistentCollection IPersistentVector
                                             ITransientCollection ITransientVector
                                             ISeq ISeqable ISequential IIndexed]]
            [clojure.lang.thread     :refer [thread-reference]]))

(declare make-vector-seq)

(deftype ^:private ChunkedSeq [-first -arr -length -position]
  ICounted
  (-count [this] -length)

  ISequential

  ISeq
  (-first [this] -first)

  (-next [this]
    (make-vector-seq -arr (dec -length) (inc -position)))

  (-more [this]
    (make-vector-seq -arr (dec -length) (inc -position)))

  ISeqable
  (-seq [this] this))

(defn- make-vector-seq [arr length position]
  (when (not= 0 length)
    (ChunkedSeq. (aget arr position) arr length position)))

(defprotocol ^:private INode
  (get-array [this])
  (get-edit [this]))

(deftype ^:private Node [-edit -arr]
  INode
  (get-array [this] -arr)

  (get-edit [this] -edit))

(defn- make-node
  ([edit arr]
   (Node. edit arr))
  ([edit]
   (Node. edit (make-array 32))))

(defn- tailoff [length]
  (if (< length 32)
    0
    (-> (->bitnum (dec length))
        (unsigned-bit-shift-right (->bitnum 5))
        (bit-shift-left (->bitnum 5)))))

(defn- new-path [edit level node]
  (if (= level 0)
    node
    (let [new-node (make-node edit)]
      (aset (get-array new-node) 0 (new-path edit (- (->bitnum level) (->bitnum 5)) node))
      new-node)))

(defn- push-tail [level parent-node tail-node length root]
  (let [subidx (bit-and (->int (unsigned-bit-shift-right (->bitnum (dec length)) (->bitnum level))) (->bitnum 0x01f))
        new-node (make-node (get-edit parent-node) (aclone (get-array parent-node)))]
    (if (= level 5)
      (aset (get-array new-node) subidx tail-node)
      (let [child (aget (get-array parent-node) subidx)]
        (if child
          (aset (get-array new-node) subidx (push-tail (- (->bitnum level) (->bitnum 5)) child tail-node length root))
          (aset (get-array new-node) subidx (new-path (get-edit root) (- (->bitnum level) (->bitnum 5)) tail-node)))))
    new-node))

(defn- transient-pop-tail [level node root length]
  (let [subidx (bit-and (->int (unsigned-bit-shift-right (->bitnum (- length 2)) (->bitnum level))) (->bitnum 0x01f))]
    (cond
      (> level 5)
        (let [new-child (transient-pop-tail (- level 5) (aget (get-array node) subidx) root length)]
          (if (and (nil? new-child) (zero? subidx))
            nil
            (let [ret (make-node (get-edit root) (aclone (get-array node)))]
              (aset (get-array ret) subidx new-child)
              ret)))
      (zero? subidx)
        nil
      :else
        (let [ret (make-node (get-edit root) (aclone (get-array node)))]
          (aset (get-array ret) subidx nil)
          ret))))

(defn- do-assoc [level node n x]
  (let [new-node (make-node (get-edit node) (aclone (get-array node)))]
    (if (= level 0)
      (do
        (aset (get-array new-node) (bit-and (->bitnum n) (->bitnum 0x01f)) x)
        new-node)
      (let [subidx (bit-and (unsigned-bit-shift-right (->bitnum n) (->bitnum level)) (->bitnum 0x01f))]
        (aset (get-array new-node) subidx (do-assoc (- level 5) (aget (get-array node) subidx) n x))
        new-node))))

(defn- ensure-editable
  ([root]
    (if (nil? (get-edit root))
      (throw (new-illegal-access-error "Transient used after persistent! call"))))
  ([root node]
    (if (= (get-edit root) (get-edit node))
      node
      (Node. (get-edit root) (aclone (get-array node))))))

(defn- editable-array-for [i length tail root shift]
  (if (and (>= i 0) (< i length))
    (if (> i (tailoff length))
      tail
      (loop [level shift
             node root]
        (if (> level 0)
          (recur
            (- level 5)
            (ensure-editable root (aget (get-array node) (bit-and (unsigned-bit-shift-right (->bitnum i) (->bitnum level)) (->bitnum 0x01f)))))
          (get-array node))))
    (throw (new-out-of-bounds-exception))))

(declare make-vector)
(declare make-transient-vec)

(deftype ^:private TransientVector [-meta
                                    ^:unsynchronized-mutable -length
                                    ^:unsynchronized-mutable -shift
                                    ^:unsynchronized-mutable -root
                                    ^:unsynchronized-mutable -tail]
  ICounted
  (-count [this]
    (ensure-editable -root)
    -length)

  ITransientCollection
  (-conj! [this x]
    (ensure-editable -root)
    (if (< (- (->bitnum -length) (->bitnum (tailoff -length))) 32)
      (do
        (aset -tail (bit-and (->bitnum -length) (->bitnum 0x01f)) x)
        (set! -length (inc -length))
        this)
      (let [tail-node (make-node (get-edit -root) -tail)
            new-tail (make-array 32)]
        (aset new-tail 0 x)
        (if (> (unsigned-bit-shift-right (->bitnum -length) (->bitnum 5)) (bit-shift-left (->bitnum 1) (->bitnum -shift)))
          (let [new-root (make-node (get-edit -root))]
            (aset (get-array new-root) 0 -root)
            (aset (get-array new-root) 1 (new-path (get-edit -root) -shift tail-node))
            (set! -length (inc -length))
            (set! -shift (+ (->bitnum -shift) (->bitnum 5)))
            (set! -root new-root)
            (set! -tail new-tail)
            this)
          (let [new-root (push-tail -shift -root tail-node -length -root)]
            (set! -root new-root)
            (set! -tail new-tail)
            (set! -length (inc -length))
            this)))))

  (-persistent [this]
    (ensure-editable -root)
    (set! -root (make-node nil (get-array -root)))
    (let [trimmed-tail (make-array (- (->bitnum -length) (->bitnum (tailoff -length))))]
      (acopy -tail 0 trimmed-tail 0 (alength trimmed-tail))
      (make-vector -meta -length -shift -root trimmed-tail)))

  ITransientVector
  (-assoc! [this index value]
    (ensure-editable -root)
    (if (and (>= index 0) (< index -length))
      (if (>= index (tailoff -length))
        (do (aset -tail (bit-and (->bitnum index) (->bitnum 0x01f)) value) this)
        (do (set! -root (do-assoc -shift -root index value)) this))
      (if (= index -length)
        (do (-conj! this value) this)
        (throw (new-out-of-bounds-exception)))))

  (-pop! [this]
    (ensure-editable -root)
    (cond
      (zero? -length)
        (throw (new-illegal-state-error "Can't pop empty vector"))
      (= 1 -length)
        (do
          (set! -length 0)
          this)
      (> (bit-and (->bitnum (dec -length)) (->bitnum 0x01f)) 0)
        (do
          (set! -length (dec -length))
          this)
      :else
        (let [new-tail (editable-array-for (- -length 2) -length -tail -root -shift)
              new-root (transient-pop-tail -shift -root -root -length)]
          (if (nil? new-root)
            (set! -root (make-node (get-edit -root))))
          (if (and (> -shift 5) (nil? (aget (get-array new-root) 1)))
            (do
              (set! -root (ensure-editable -root (aget (get-array new-root) 0)))
              (set! -shift (- -shift 5))))
          (set! -length (dec -length))
          (set! -tail new-tail)
          this)))

)

(defn- editable-root [root]
  (make-node (thread-reference) (aclone (get-array root))))

(defn- editable-tail [tail]
  (let [new-arr (make-array 32)]
    (acopy tail 0 new-arr 0 (alength tail))
    new-arr))

(defn n-in-range? [n length]
  (and (>= (->bitnum n) 0) (< (->bitnum n) (->bitnum length))))

(defn- make-transient-vec [meta length shift root tail]
  (TransientVector. meta length shift (editable-root root) (editable-tail tail)))

(declare EMPTY-VECTOR)

(deftype PersistentVector [-meta -length -shift -root -tail -seq]
  IPersistentCollection
  (-cons [this x]
    (if (< (- (->bitnum -length) (->bitnum (tailoff -length))) 32)
      (let [tail-length (alength -tail)
            new-tail (make-array (inc tail-length))]
        (acopy -tail 0 new-tail 0 tail-length)
        (aset new-tail tail-length x)
        (make-vector -meta (inc -length) -shift -root new-tail))
      (let [tail-node (make-node (get-edit -root) -tail)
            new-arr (make-array 1)]
        (aset new-arr 0 x)
        (if (> (unsigned-bit-shift-right (->bitnum -length) (->bitnum 5)) (bit-shift-left (->bitnum 1) (->bitnum -shift)))
          (let [new-root (make-node (get-edit -root))]
            (aset (get-array new-root) 0 -root)
            (aset (get-array new-root) 1 (new-path (get-edit -root) -shift tail-node))
            (make-vector -meta (inc -length) (+ (->bitnum -shift) (->bitnum 5)) new-root new-arr))
          (let [new-root (push-tail -shift -root tail-node -length -root)]
            (make-vector -meta (inc -length) -shift new-root new-arr))))))

  (-empty [this]
    (with-meta EMPTY-VECTOR (meta this)))

  IPersistentVector
  (-assoc-n [this n x]
    (cond
      (n-in-range? n -length)
      (if (>= n (tailoff -length))
        (let [tail-length (alength -tail)
              new-tail (make-array tail-length)]
          (acopy -tail 0 new-tail 0 tail-length)
          (aset new-tail (bit-and (->bitnum n) (->bitnum 0x01f)) x)
          (make-vector -meta -length -shift -root new-tail))
        (make-vector -meta -length -shift (do-assoc -shift -root n x) -tail))
      (= n -length)
      (cons this x)
      :else
      (throw (new-out-of-bounds-exception ""))))

  IAssociative
  (-assoc [this k v]
    (if (integer? k)
      (-assoc-n this k v)
      (throw (new-argument-error "Key must be an integer"))))

  (-contains-key? [this k]
    (if (integer? k)
      (n-in-range? k -length)
      false))

  ICounted
  (-count [this] -length)

  IEditableCollection
  (-as-transient [this]
    (make-transient-vec -meta -length -shift -root -tail))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this new-meta]
    (make-vector new-meta -length -shift -root -tail))

  ISeqable
  (-seq [this] -seq)

  IIndexed
  (-nth [this n]
    (aget -tail n))

  (-nth [this n not-found]
    (if (n-in-range? n -length)
      (nth this n)
      not-found))
)

(defn- make-vector [meta length shift root arr]
  (PersistentVector. meta length shift root arr (make-vector-seq arr length 0)))

(def ^:private EMPTY-NODE (make-node nil (make-array 32)))

(def ^:private EMPTY-ARRAY (make-array 0))

(def EMPTY-VECTOR (make-vector nil 0 5 EMPTY-NODE EMPTY-ARRAY))

