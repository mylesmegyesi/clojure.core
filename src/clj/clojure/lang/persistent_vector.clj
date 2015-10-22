(ns clojure.lang.persistent-vector
  (:refer-clojure :only [cond declare defn- defn defprotocol if-let let loop when ->])
  (:require [clojure.next :refer :all :exclude [bit-shift-left unsigned-bit-shift-right]]
            [clojure.lang
              [array-chunk     :refer [make-array-chunk]]
              [aseq            :refer [defseq seq->array]]
              [collection      :as    coll]
              [deftype         :refer [deftype]]
              [exceptions      :refer [new-argument-error new-out-of-bounds-exception
                                       new-illegal-access-error new-illegal-state-error
                                       new-unsupported-error]]
              [numbers         :refer [->int platform-long platform-big-int platform-big-integer]]
              [hash-map        :refer [->bitnum bit-shift-left unsigned-bit-shift-right]]
              [persistent-list :refer [EMPTY-LIST]]
              [protocols       :refer [-as-transient -assoc-n -assoc-n! -array-for
                                       -conj! -count -persistent -lookup -nth
                                       -chunked-first -chunked-next -chunked-more
                                       IAssociative ICounted IEditableCollection IMeta IObj ILookup
                                       IPersistentCollection IPersistentVector IPersistentStack
                                       ITransientAssociative ITransientCollection ITransientVector
                                       IChunkedSeq ISeq ISeqable ISequential IIndexed]]
              [thread          :refer [thread-reference]]]))

(coll/import-collection-type)

(declare make-chunked-seq)

(declare EMPTY-VECTOR)
(declare EMPTY-NODE)

(defseq ChunkedSeq [-vec -node -i -offset -meta]
  IChunkedSeq
  (-chunked-first [this]
    (make-array-chunk -node -offset))

  (-chunked-next [this]
    (when (< (+ -i (alength -node)) (count -vec))
      (let [next-i (+ -i (alength -node))]
        (make-chunked-seq -vec (-array-for -vec next-i) next-i 0 nil))))

  (-chunked-more [this]
    (if-let [s (-chunked-next this)]
      s
      EMPTY-LIST))

  ICounted
  (-count [this]
    (- (count -vec) (+ -i -offset)))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this mta]
    (if (= mta -meta)
      this
      (make-chunked-seq -vec -node -i -offset mta)))

  IPersistentCollection
  (-cons [this x]
    (cons x this))

  (-empty [this] EMPTY-LIST)

  ISeq
  (-first [this]
    (aget -node -offset))

  (-next [this]
    (if (< (inc -offset) (alength -node))
      (make-chunked-seq -vec -node -i (inc -offset) nil)
      (-chunked-next this)))

  (-more [this]
    (if-let [s (next this)]
      s
      EMPTY-LIST)))

(defn is-chunked-seq? [cs]
  (instance? ChunkedSeq cs))

(defn- make-chunked-seq [vc node i offset mta]
  (ChunkedSeq. vc node i offset mta))

(defn n-in-range? [n length]
  (and (>= (->bitnum n) 0) (< (->bitnum n) (->bitnum length))))

(declare make-vector-seq)

(defseq VectorSeq [-v -i -meta]
  ICounted
  (-count [this]
    (- (count -v) -i))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (make-vector-seq -v -i m))

  ISeq
  (-first [this]
    (nth -v -i))

  (-next [this]
    (if (< (inc -i) (count -v))
      (make-vector-seq -v (inc -i) nil)
      nil))

  (-more [this]
    (if (< (inc -i) (count -v))
      (make-vector-seq -v (inc -i) nil)
      EMPTY-LIST)))

(defn make-vector-seq [v i mta]
  (VectorSeq. v i mta))

(declare make-subvec)

(deftype SubVector [-v -start -end -meta]
  IAssociative
  (-assoc [this k v]
    (if (integer? k)
      (-assoc-n this k v)
      (throw (new-argument-error "Key must be an integer"))))

  (-contains-key? [this k]
    (if (integer? k)
      (n-in-range? k (count this))
      false))

  ICounted
  (-count [this]
    (- -end -start))

  IIndexed
  (-nth [this n]
    (if (or (>= (+ -start n) -end) (< n 0))
      (throw (new-out-of-bounds-exception))
      (-nth -v (+ -start n))))

  (-nth [this n not-found]
    (if (or (>= (+ -start n) -end) (< n 0))
      not-found
      (-nth -v (+ -start n))))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (if (= m -meta)
      this
      (make-subvec -v -start -end m)))

  IPersistentCollection
  (-cons [this x]
    (make-subvec (-assoc-n -v -end x) -start (inc -end) -meta))

  (-empty [this]
    (with-meta EMPTY-VECTOR -meta))

  IPersistentStack
  (-peek [this]
    (if (> (count this) 0)
      (nth this (dec (count this)))
      nil))

  (-pop [this]
    (if (= -start (dec -end))
      EMPTY-VECTOR
      (make-subvec -v -start (dec -end) -meta)))

  IPersistentVector
  (-assoc-n [this n x]
    (cond
      (< -end (+ n -start))
        (throw (new-out-of-bounds-exception))
      (= -end (+ n -start))
        (conj this x)
      :else
        (make-subvec (-assoc-n -v (+ -start n) x) -start (inc -end) -meta)))

  ISeqable
  (-seq [this]
    (if (> (count this) 0)
      (make-vector-seq this 0 nil)
      nil))

  coll/base-collection
  (coll/add-method [this o]
    (throw (new-unsupported-error)))

  (coll/add-all-method [this os]
    (throw (new-unsupported-error)))

  (coll/clear-method [this]
    (throw (new-unsupported-error)))

  (coll/contains?-method [this o]
    (loop [s (seq this)]
      (if s
        (if (= (first s) o)
          true
          (recur (next s)))
        false)))

  (coll/contains-all?-method [this os]
    (loop [o (seq os)]
      (if o
        (if (not (coll/contains? this (first o)))
          false
          (recur (next o)))
        true)))

  (coll/is-empty?-method [this]
    (zero? (count this)))

  (coll/remove-method [this o]
    (throw (new-unsupported-error)))

  (coll/remove-all-method [this os]
    (throw (new-unsupported-error)))

  (coll/retain-all-method [this os]
    (throw (new-unsupported-error)))

  (coll/size-method [this]
    (count this))

  (coll/to-array-method [this]
    (seq->array (seq this)))

  (coll/to-array-method [this arr]
    (seq->array (seq this) arr)))

(defn make-subvec [v start end mta]
  (if (instance? SubVector v)
    (let [v (.vector v)
          s (+ start (.start v))
          e (+ end (.end v))]
      (SubVector. v s e mta))
    (SubVector. v start end mta)))

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
   (Node. edit (object-array 32))))

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

(defn- pop-tail [level node root length]
  (let [subidx (bit-and (->int (unsigned-bit-shift-right (->bitnum (- length 2)) (->bitnum level))) (->bitnum 0x01f))]
    (cond
      (> level 5)
        (let [new-child (pop-tail (- level 5) (aget (get-array node) subidx) root length)]
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

(defn- array-for [i length tail root shift]
  (if (and (>= i 0) (< i length))
    (if (>= i (tailoff length))
      tail
      (loop [node root
             level shift]
        (let [l (- level 5)]
          (if (> 0 l)
            (recur
              (aget (get-array node) (bit-and (unsigned-bit-shift-right (->bitnum i) (->bitnum l)) (->bitnum 0x01f)))
              l)
            (get-array node)))))
    (throw (new-out-of-bounds-exception))))

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

(defn- is-integer? [i]
  (or (integer? i)
      (instance? platform-long i)
      (instance? platform-big-int i)
      (instance? platform-big-integer i)))

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

  ILookup
  (-lookup [this k not-found]
    (ensure-editable -root)
    (if (is-integer? k)
      (let [i (int k)]
        (if (and (>= i 0) (< i -length))
          (-nth this i)
          not-found)
      not-found)))

  IIndexed
  (-nth [this n]
    (ensure-editable -root)
    (let [node (array-for n -length -tail -root -shift)]
      (aget node (bit-and (->bitnum n) (->bitnum 0x01f)))))

  (-nth [this n not-found]
    (if (and (>= n 0) (< n (-count this)))
      (-nth this n)
      not-found))

  ITransientAssociative
  (-assoc! [this k v]
    (if (is-integer? k)
      (-assoc-n! this (int k) v)
      (throw (new-argument-error "Key must be integer"))))

  ITransientCollection
  (-conj! [this x]
    (ensure-editable -root)
    (if (< (- (->bitnum -length) (->bitnum (tailoff -length))) 32)
      (do
        (aset -tail (bit-and (->bitnum -length) (->bitnum 0x01f)) x)
        (set! -length (inc -length))
        this)
      (let [tail-node (make-node (get-edit -root) -tail)
            new-tail (object-array 32)]
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
    (let [trimmed-tail (object-array (- (->bitnum -length) (->bitnum (tailoff -length))))]
      (acopy -tail 0 trimmed-tail 0 (alength trimmed-tail))
      (make-vector -meta -length -shift -root trimmed-tail)))

  ITransientVector
  (-assoc-n! [this index value]
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
              new-root (pop-tail -shift -root -root -length)]
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
  (let [new-arr (object-array 32)]
    (acopy tail 0 new-arr 0 (alength tail))
    new-arr))

(defn- make-transient-vec [meta length shift root tail]
  (TransientVector. meta length shift (editable-root root) (editable-tail tail)))

(deftype PersistentVector [-meta -length -shift -root -tail]
  IPersistentCollection
  (-cons [this x]
    (if (< (- (->bitnum -length) (->bitnum (tailoff -length))) 32)
      (let [tail-length (alength -tail)
            new-tail (object-array (inc tail-length))]
        (acopy -tail 0 new-tail 0 tail-length)
        (aset new-tail tail-length x)
        (make-vector -meta (inc -length) -shift -root new-tail))
      (let [tail-node (make-node (get-edit -root) -tail)
            new-arr (object-array 1)]
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

  IPersistentStack
  (-peek [this]
    (if (> -length 0)
      (nth this (dec -length))
      nil))

  (-pop [this]
    (cond
      (zero? -length) (throw (new-illegal-state-error "Can't pop empty vector"))
      (= 1 -length) (with-meta EMPTY-VECTOR -meta)
      (< 1 (- -length (tailoff -length)))
        (let [new-tail (object-array (dec (alength -tail)))]
          (acopy -tail 0 new-tail 0 (alength new-tail))
          (make-vector -meta (dec -length) -shift -root new-tail))
      :else
        (let [new-tail (array-for (- -length 2) -length -tail -root -shift)
              new-root (pop-tail -shift -root -root -length)]
          (cond
            (nil? new-root)
              (make-vector -meta (dec -length) -shift EMPTY-NODE new-tail)
            (and (> -shift 5) (nil? (aget (get-array new-root) 1)))
              (make-vector -meta (dec -length) (- -shift 5) (aget (get-array new-root) 0) new-tail)
            :else
              (make-vector -meta (dec -length) -shift new-root new-tail)))))

  IPersistentVector
  (-assoc-n [this n x]
    (cond
      (n-in-range? n -length)
      (if (>= n (tailoff -length))
        (let [tail-length (alength -tail)
              new-tail (object-array tail-length)]
          (acopy -tail 0 new-tail 0 tail-length)
          (aset new-tail (bit-and (->bitnum n) (->bitnum 0x01f)) x)
          (make-vector -meta -length -shift -root new-tail))
        (make-vector -meta -length -shift (do-assoc -shift -root n x) -tail))
      (= n -length)
      (cons this x)
      :else
      (throw (new-out-of-bounds-exception ""))))

  ; private method for internal use
  (-array-for [this i]
    (array-for i -length -tail -root -shift))

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
  (-seq [this]
    (if (zero? (count this))
      nil
      (let [arr-for (array-for 0 -length -tail -root -shift)]
        (make-chunked-seq this arr-for 0 0 nil))))

  IIndexed
  (-nth [this n]
    (aget -tail n))

  (-nth [this n not-found]
    (if (n-in-range? n -length)
      (nth this n)
      not-found))

  coll/base-collection
  (coll/add-method [this o]
    (throw (new-unsupported-error)))

  (coll/add-all-method [this os]
    (throw (new-unsupported-error)))

  (coll/clear-method [this]
    (throw (new-unsupported-error)))

  (coll/contains?-method [this o]
    (loop [s (seq this)]
      (if s
        (if (= (first s) o)
          true
          (recur (next s)))
        false)))

  (coll/contains-all?-method [this os]
    (loop [o (seq os)]
      (if o
        (if (not (coll/contains? this (first o)))
          false
          (recur (next o)))
        true)))

  (coll/is-empty?-method [this]
    (zero? (count this)))

  (coll/remove-method [this o]
    (throw (new-unsupported-error)))

  (coll/remove-all-method [this os]
    (throw (new-unsupported-error)))

  (coll/retain-all-method [this os]
    (throw (new-unsupported-error)))

  (coll/size-method [this]
    (count this))

  (coll/to-array-method [this]
    (seq->array (seq this)))

  (coll/to-array-method [this arr]
    (seq->array (seq this) arr)))

(defn- make-vector [meta length shift root arr]
  (PersistentVector. meta length shift root arr))

(def ^:private EMPTY-NODE (make-node nil (object-array 32)))

(def ^:private EMPTY-ARRAY (object-array 0))

(def EMPTY-VECTOR (make-vector nil 0 5 EMPTY-NODE EMPTY-ARRAY))

