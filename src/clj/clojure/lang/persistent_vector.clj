(ns clojure.lang.persistent-vector
  (:refer-clojure :only [cond declare defn- defn defprotocol deftype dec inc let loop when < > >= ->])
  (:require [clojure.next                   :refer :all :exclude [dec inc]]
            [clojure.lang.exceptions        :refer [out-of-bounds-exception]]
            [clojure.lang.numbers           :refer [->int]]
            [clojure.lang.platform.hash-map :refer [->bitnum]]
            [clojure.lang.protocols         :refer [-as-transient -conj! -nth -persistent -cons ICounted IEditableCollection IMeta IPersistentVector ISeq ISeqable ITransientCollection Indexed]]))

(declare make-vector-seq)

(deftype ^:private PersistentVectorSeq [-first -arr -length -position]
  ICounted
  (-count [this]
    -length)

  ISeq
  (-first [this]
    -first)

  (-next [this]
    (make-vector-seq -arr (dec -length) (inc -position)))

  ISeqable
  (-seq [this]
    this)
)

(defn- make-vector-seq [arr length position]
  (when (not= 0 length)
    (PersistentVectorSeq. (aget arr position) arr length position)))

(defprotocol ^:private INode
  (get-array [this])
  (get-edit [this]))

(deftype ^:private Node [-edit -arr]
  INode
  (get-array [this]
    -arr)

  (get-edit [this]
    -edit)
)

(defn- make-node
  ([edit arr]
   (Node. edit arr))
  ([edit]
   (Node. edit (make-array 32))))

(defn- tailoff [length]
  (if (< length 32)
    0
    (-> (->bitnum (dec length))
        (bit-unsigned-shift-right (->bitnum 5))
        (bit-shift-left (->bitnum 5)))))

(defn- new-path [edit level node]
  (if (= level 0)
    node
    (let [new-node (make-node edit)]
      (aset (get-array new-node) 0 (new-path edit (- (->bitnum level) (->bitnum 5)) node))
      new-node)))

(defn- push-tail [level parent-node tail-node length root]
  (let [subidx (bit-and (->int (bit-unsigned-shift-right (->bitnum (dec length)) (->bitnum level))) (->bitnum 0x01f))
        new-node (make-node (get-edit parent-node) (aclone (get-array parent-node)))]
    (if (= level 5)
      (aset (get-array new-node) subidx tail-node)
      (let [child (aget (get-array parent-node) subidx)]
        (if child
          (aset (get-array new-node) subidx (push-tail (- (->bitnum level) (->bitnum 5)) child tail-node length root))
          (aset (get-array new-node) subidx (new-path (get-edit root) (- (->bitnum level) (->bitnum 5)) tail-node)))))
    new-node))

(declare make-vector)
(declare make-transient-vec)

(deftype ^:private TransientVector [-meta -length -shift -root -tail]
  ITransientCollection
  (-conj! [this x]
    (if (< (- (->bitnum -length) (->bitnum (tailoff -length))) 32)
      (do
        (aset -tail (bit-and (->bitnum -length) (->bitnum 0x01f)) x)
        (make-transient-vec -meta (inc -length) -shift -root -tail))
      (let [tail-node (make-node (get-edit -root) -tail)
            new-tail (make-array 32)]
        (aset new-tail 0 x)
        (if (> (bit-unsigned-shift-right (->bitnum -length) (->bitnum 5)) (bit-shift-left (->bitnum 1) (->bitnum -shift)))
          (let [new-root (make-node (get-edit -root))]
            (aset (get-array new-root) 0 -root)
            (aset (get-array new-root) 1 (new-path (get-edit -root) -shift tail-node))
            (make-transient-vec -meta (inc -length) (+ (->bitnum -shift) (->bitnum 5)) new-root new-tail))
          (let [new-root (push-tail -shift -root tail-node -length -root)]
            (make-transient-vec -meta (inc -length) -shift new-root new-tail))))))

  (-persistent [this]
    (let [trimmed-tail (make-array (- (->bitnum -length) (->bitnum (tailoff -length))))]
      (acopy -tail 0 trimmed-tail 0 (alength trimmed-tail))
      (make-vector -meta -length -shift -root trimmed-tail)))
)

(defn- editable-root [root]
  (make-node nil (aclone (get-array root))))

(defn- editable-tail [tail]
  (let [new-arr (make-array 32)]
    (acopy tail 0 new-arr 0 (alength tail))
    new-arr))

(defn n-in-range? [n length]
  (and (> (->bitnum n) 0) (< (->bitnum n) (->bitnum length))))

(defn- make-transient-vec [meta length shift root tail]
  (TransientVector. meta length shift (editable-root root) (editable-tail tail)))

(defn- do-assoc [level node n x]
  (let [new-node (make-node (get-edit node) (aclone (get-array node)))]
    (if (= level 0)
      (do
        (aset (get-array new-node) (bit-and (->bitnum n) (->bitnum 0x01f)) x)
        new-node)
      (let [subidx (bit-and (bit-unsigned-shift-right (->bitnum n) (->bitnum level)) (->bitnum 0x01f))]
        (aset (get-array new-node) subidx (do-assoc (- level 5) (aget (get-array node) subidx) n x))
        new-node))))

(deftype PersistentVector [-meta -length -shift -root -tail -seq]
  IPersistentVector
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
        (if (> (bit-unsigned-shift-right (->bitnum -length) (->bitnum 5)) (bit-shift-left (->bitnum 1) (->bitnum -shift)))
          (let [new-root (make-node (get-edit -root))]
            (aset (get-array new-root) 0 -root)
            (aset (get-array new-root) 1 (new-path (get-edit -root) -shift tail-node))
            (make-vector -meta (inc -length) (+ (->bitnum -shift) (->bitnum 5)) new-root new-arr))
          (let [new-root (push-tail -shift -root tail-node -length -root)]
            (make-vector -meta (inc -length) -shift new-root new-arr))))))

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
      (-cons this x)
      :else
      out-of-bounds-exception))

  ICounted
  (-count [this]
    -length)

  IEditableCollection
  (-as-transient [this]
    (make-transient-vec -meta -length -shift -root -tail))

  IMeta
  (-meta [this]
    -meta)

  (-with-meta [this new-meta]
    (make-vector new-meta -length -shift -root -tail))

  ISeqable
  (-seq [this]
    -seq)

  Indexed
  (-nth [this n]
    (aget -tail n))

  (-nth [this n not-found]
    (if (n-in-range? n -length)
      (-nth this n)
      not-found))
)

(defn- make-vector [meta length shift root arr]
  (PersistentVector. meta length shift root arr (make-vector-seq arr length 0)))

(def ^:private EMPTY-NODE (make-node nil (make-array 32)))

(def ^:private EMPTY-ARRAY (make-array 0))

(def ^:private EMPTY (make-vector nil 0 5 EMPTY-NODE EMPTY-ARRAY))

(defn vector [& args]
  (let [arg-seq (seq args)
        empty-transient (-as-transient EMPTY)]
    (if arg-seq
      (loop [xs arg-seq v empty-transient]
        (if xs
          (recur (next xs) (-conj! v (first xs)))
          (-persistent v)))
      (-persistent empty-transient))))
