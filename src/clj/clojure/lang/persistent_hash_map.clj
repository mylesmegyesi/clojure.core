(ns clojure.lang.persistent-hash-map
  (:refer-clojure :only [defn defn- declare defprotocol let if-let when loop cond ->])
  (:require [clojure.lang
              [apersistent-map :refer [map-cons map-equals? map-hash]]
              [aseq            :refer [defseq]]
              [atomic-ref      :refer [new-atomic-ref ref-get ref-set!]]
              [deftype         :refer [deftype]]
              [enumerable      :as    enum]
              [equivalence     :as    equiv]
              [exceptions      :refer [new-argument-error new-illegal-access-error]]
              [hash            :as    hash-code]
              [key-value       :refer [platform-map-entry-type]]
              [map-entry       :refer [new-map-entry]]
              [object          :as    obj]
              [persistent-list :refer [EMPTY-LIST]]
              [hash-map        :refer [->bitnum empty-object
                                       bit-and bit-or bit-xor bit-shift-left unsigned-bit-shift-right bit-count
                                       + inc * - dec]]
              [thread          :refer [thread-reference]]
              [protocols       :refer [IAssociative ICounted ILookup IFn IEditableCollection
                                       IMeta IObj IPersistentCollection IPersistentMap
                                       ISeqable ISeq ISeqable ISequential
                                       ITransientAssociative ITransientCollection ITransientMap
                                       -assoc! -lookup]]]
            [clojure.next :refer :all :exclude [bit-and bit-or bit-xor bit-shift-left
                                                unsigned-bit-shift-right + inc * - dec]]))

(def ^:private NEG-ONE    (->bitnum -1))
(def ^:private ZERO       (->bitnum 0))
(def ^:private ONE        (->bitnum 1))
(def ^:private TWO        (->bitnum 2))
(def ^:private THREE      (->bitnum 3))
(def ^:private FOUR       (->bitnum 4))
(def ^:private FIVE       (->bitnum 5))
(def ^:private SIXTEEN    (->bitnum 16))
(def ^:private THIRTY-TWO (->bitnum 32))
(def ^:private BITMASK    (->bitnum 0x01f))

(defprotocol ^:private IBoxedValue
  (get-value  [this])
  (set-value! [this new-val]))

(deftype ^:private BoxedValue [^:unsynchronized-mutable val]
  IBoxedValue
  (get-value [this] val)
  (set-value! [this new-val] (set! val new-val)))

(defn- new-box [val]
  (BoxedValue. val))

(defprotocol ^:private INode
  (ensure-editable [this edit])
  (get-array       [this])
  (node-assoc      [this shift hash key val added-leaf])
  (node-assoc-ref  [this edit shift hash key val added-leaf])
  (node-dissoc     [this shift hash key])
  (node-dissoc-ref [this edit shift hash key removed-leaf])
  (node-find       [this shift hash key not-found])
  (node-seq        [this])
  (set-array!      [this new-arr]))

(defprotocol ^:private Bitmapped
  (get-bitmap  [this])
  (set-bitmap! [this new-bitmap]))

(declare EMPTY-BitmapIndexedNode)
(declare new-bitmap-node)
(declare new-array-node)
(declare new-hash-collision-node)
(declare new-hash-map)
(declare new-node-seq)

(defn- bit-index [bitmap bit]
  (bit-count (bit-and bitmap (dec bit))))

(defn- mask [hash shift]
  (bit-and
    (unsigned-bit-shift-right hash shift)
    BITMASK))

(defn- bit-pos [hash shift]
  (bit-shift-left ONE (mask hash shift)))

(defn- edit-and-set!
  ([this edit i a]
   (let [editable (ensure-editable this edit)
         editable-array (get-array editable)]
     (aset editable-array i a)
     editable))
  ([this edit i a j b]
   (let [editable (ensure-editable this edit)
         editable-array (get-array editable)]
     (aset editable-array i a)
     (aset editable-array j b)
     editable)))

(defn- clone-and-set!
  ([arr i a]
    (let [clone (aclone arr)]
      (aset clone i a)
      clone))
  ([arr i a j b]
   (let [clone (aclone arr)]
     (aset clone i a)
     (aset clone j b)
     clone)))

(defn- create-node
  ([shift key1 val1 key2hash key2 val2]
   (let [key1hash (->bitnum (hash key1))]
     (if (= key1hash key2hash)
       (let [arr (object-array FOUR)]
         (aset arr ZERO key1)
         (aset arr ONE val1)
         (aset arr TWO key2)
         (aset arr THREE val2)
         (new-hash-collision-node nil key1hash TWO arr))
       (let [added-leaf (new-box nil)
             edit (new-atomic-ref)]
         (-> EMPTY-BitmapIndexedNode
           (node-assoc-ref edit shift key1hash key1 val1 added-leaf)
           (node-assoc-ref edit shift key2hash key2 val2 added-leaf))))))
  ([edit shift key1 val1 key2hash key2 val2]
   (let [key1hash (->bitnum (hash key1))]
     (if (= key1hash key2hash)
       (let [arr (object-array FOUR)]
         (aset arr ZERO key1)
         (aset arr ONE val1)
         (aset arr TWO key2)
         (aset arr THREE val2)
         (new-hash-collision-node nil key1hash TWO arr))
       (let [added-leaf (new-box nil)]
         (-> EMPTY-BitmapIndexedNode
           (node-assoc-ref edit shift key1hash key1 val1 added-leaf)
           (node-assoc-ref edit shift key2hash key2 val2 added-leaf)))))))

(defn- find-index [array count key]
  (loop [i ZERO]
    (if (< i (* TWO count))
      (if (= key (aget array i))
        i
        (recur (+ i TWO)))
      NEG-ONE)))

(defn- remove-pair [arr i]
  (let [new-size (- (alength arr) TWO)
        new-arr (object-array new-size)
        two*i (* TWO i)]
    (acopy arr ZERO new-arr ZERO two*i)
    (acopy arr (* TWO (inc i)) new-arr two*i (- new-size two*i))
    new-arr))

(defn- edit-and-remove-pair [this edit bit i]
  (if (= (get-bitmap this) bit)
    nil
    (let [editable (ensure-editable this edit)
          arr (get-array editable)]
      (set-bitmap! (get-bitmap editable) (bit-or (get-bitmap editable) bit))
      (acopy arr (* 2 (inc i)) arr (* i 2) (- (alength arr) (* 2 (inc i))))
      (aset arr (- (alength arr) 2) nil)
      (aset arr (dec (alength arr)) nil)
      editable)))

(defseq ^:private NodeSeq [meta arr i s]
  ICounted
  (-count [this]
    (let [s (next this)]
      (if s
        (inc (count s))
        ONE)))

  ISeq
  (-first [this]
    (if (nil? s)
      (new-map-entry (aget arr i) (aget arr (inc i)))
      (first s)))

  (-next [this]
    (if (nil? s)
      (new-node-seq arr (+ TWO i) nil)
      (new-node-seq arr i (next s))))

  (-more [this]
    (if-let [s (next this)] s EMPTY-LIST)))

(defn- new-node-seq
  ([arr] (new-node-seq arr ZERO nil))
  ([arr i s]
   (if (nil? s)
     (loop [j i max (alength arr)]
       (if (< j max)
         (if (nil? (aget arr j))
           (let [node (aget arr (inc j))]
             (if (nil? node)
               (recur (+ TWO j) max)
               (let [seq (node-seq node)]
                 (if (nil? seq)
                   (recur (+ TWO j) max)
                   (NodeSeq. nil arr (+ TWO j) seq)))))
           (NodeSeq. nil arr j nil))
         nil))
     (NodeSeq. nil arr i s))))

(deftype ^:private ArrayNode [edit count array]
  INode
  (node-find [this shift hash key not-found]
    (let [idx (mask hash shift)
          node (aget array idx)]
      (if (nil? node)
        not-found
        (node-find node (+ shift FIVE) hash key not-found))))

  (node-assoc [this shift hash key val added-leaf]
    (let [idx (mask hash shift)
          node (aget array idx)]
      (if (nil? node)
        (new-array-node nil (inc count) (clone-and-set!
                                          array idx
                                          (node-assoc EMPTY-BitmapIndexedNode
                                                      (+ shift FIVE)
                                                      hash key val added-leaf)))
        (let [n (node-assoc node (+ shift FIVE) hash key val added-leaf)]
          (if (= n node)
            this
            (new-array-node nil count (clone-and-set! array idx n)))))))
  )

(defn- new-array-node [edit count array]
  (ArrayNode. edit count array))

(deftype ^:private HashCollisionNode [edit -hash count array]
  INode
  (node-find [this shift hash key not-found]
    (let [idx (find-index array count key)]
      (cond
        (< idx ZERO)
        not-found
        (= key (aget array idx))
        (aget array (inc idx))
        :else
        not-found)))

  (node-assoc [this shift hash key val added-leaf]
    (if (= hash -hash)
      (let [idx (find-index array count key)]
        (if (not= idx NEG-ONE)
          (if (= (aget array (inc idx)) val)
            this
            (new-hash-collision-node nil hash count (clone-and-set! array (inc idx) val)))
          (let [new-arr (object-array (* TWO (inc count)))]
            (acopy array ZERO new-arr ZERO (* TWO count))
            (aset new-arr (* TWO count) key)
            (aset new-arr (inc (* TWO count)) val)
            (set-value! added-leaf added-leaf)
            (new-hash-collision-node edit hash (inc count) new-arr))))
      (let [new-arr (object-array TWO)]
        (aset new-arr ZERO nil)
        (aset new-arr ONE this)
        (node-assoc (new-bitmap-node nil (bit-pos hash shift) new-arr)
                    shift hash key val added-leaf))))
  )

(defn- new-hash-collision-node [edit hash count array]
  (HashCollisionNode. edit hash count array))

(deftype ^:private BitmapIndexedNode [edit ^:unsynchronized-mutable bitmap ^:unsynchronized-mutable arr]
  Bitmapped
  (get-bitmap  [this] bitmap)
  (set-bitmap! [this new-bitmap] (set! bitmap new-bitmap))

  INode
  (get-array  [this] arr)
  (set-array! [this new-arr] (set! arr new-arr))

  (ensure-editable [this -edit]
    (if (= edit -edit)
      this
      (let [n (bit-count bitmap)
            arr-size (if (>= n ZERO) (* TWO (inc n)) FOUR)
            new-arr (object-array arr-size)]
        (acopy arr ZERO new-arr ZERO (* TWO n))
        (new-bitmap-node -edit bitmap new-arr))))

  (node-find [this shift hash key not-found]
    (let [bit (bit-pos hash shift)]
      (if (= (bit-and bitmap bit) ZERO)
        not-found
        (let [idx (bit-index bitmap bit)
              key-or-nil (aget arr (* TWO idx))
              val-or-node (aget arr (inc (* TWO idx)))]
          (cond
            (nil? key-or-nil)
            (node-find val-or-node (+ FIVE shift) hash key not-found)
            (= key key-or-nil)
            val-or-node
            :else not-found)))))

  (node-assoc [this shift hash key val added-leaf]
    (let [bit (bit-pos hash shift)
          idx (bit-index bitmap bit)]
      (if (not= (bit-and bitmap bit) ZERO)
        (let [key-or-nil (aget arr (* TWO idx))
              val-or-node (aget arr (inc (* TWO idx)))]
          (cond
            (nil? key-or-nil)
            (let [n (node-assoc val-or-node (+ FIVE shift) hash key val added-leaf)]
              (if (= n val-or-node)
                this
                (new-bitmap-node nil bitmap (clone-and-set! arr (inc (* TWO idx)) n))))
            (= key key-or-nil)
            (if (= val val-or-node)
              this
              (new-bitmap-node nil bitmap (clone-and-set! arr (inc (* TWO idx)) val)))
            :else
            (do
              (set-value! added-leaf added-leaf)
              (new-bitmap-node nil bitmap (clone-and-set! arr
                                                          (* TWO idx) nil
                                                          (inc (* TWO idx))
                                                          (create-node (+ FIVE shift)
                                                                       key-or-nil
                                                                       val-or-node
                                                                       hash key val))))))
        (let [n (bit-count bitmap)]
          (if (>= n SIXTEEN)
            (let [nodes (object-array THIRTY-TWO)
                  jdx (mask hash shift)
                  jdx-node (node-assoc EMPTY-BitmapIndexedNode
                                       (+ shift FIVE) hash key val added-leaf)]
              (aset nodes jdx jdx-node)
              (loop [i ZERO j ZERO]
                (when (< i THIRTY-TWO)
                  (if (not= (bit-and (unsigned-bit-shift-right bitmap i) ONE) ZERO)
                    (do
                      (if (nil? (aget arr j))
                        (aset nodes i (aget arr (inc j)))
                        (aset nodes i (node-assoc EMPTY-BitmapIndexedNode
                                                        (+ shift FIVE)
                                                        (clojure.next/hash (aget arr j))
                                                        (aget arr j)
                                                        (aget arr (inc j))
                                                        added-leaf)))
                      (recur (inc i) (+ j TWO)))
                    (recur (inc i) j))))
              (new-array-node nil (inc n) nodes))
            (let [new-arr (object-array (* TWO (inc n)))]
              (acopy arr ZERO new-arr ZERO (* TWO idx))
              (aset new-arr (* TWO idx) key)
              (set-value! added-leaf added-leaf)
              (aset new-arr (inc (* TWO idx)) val)
              (acopy arr (* TWO idx) new-arr (* TWO (inc idx)) (* TWO (- n idx)))
              (new-bitmap-node nil (bit-or bitmap bit) new-arr)))))))

  (node-assoc-ref [this edit shift hash key val added-leaf]
      (let [bit (bit-pos hash shift)
            idx (bit-index bitmap bit)]
        (if (not= (bit-and bitmap bit) ZERO)
          (let [key-or-nil (aget arr (* TWO idx))
                val-or-node (aget arr (inc (* TWO idx)))]
            (cond
              (nil? key-or-nil)
              (let [n (node-assoc-ref val-or-node edit (+ FIVE shift) hash key val added-leaf)]
                (if (= n val-or-node)
                  this
                  (edit-and-set! this edit (inc (* TWO idx)) n)))
              (= key key-or-nil)
              (if (= val val-or-node)
                this
                (edit-and-set! this edit (inc (* TWO idx)) val))
              :else
              (do
                (set-value! added-leaf added-leaf)
                (edit-and-set! this edit (* TWO idx) nil (inc (* TWO idx))
                              (create-node edit (+ FIVE shift) key-or-nil val-or-node hash key val)))))
          (let [n (bit-count bitmap)]
            (cond
              (< (* TWO n) (alength arr))
              (do
                (set-value! added-leaf added-leaf)
                (let [editable (ensure-editable this edit)
                      editable-arr (get-array editable)]
                  (acopy editable-arr (* TWO idx) (get-array editable) (* TWO (inc idx)) (* TWO (- n idx)))
                  (aset editable-arr (* TWO idx) key)
                  (aset editable-arr (inc (* TWO idx)) val)
                  (set-bitmap! editable (bit-or (get-bitmap editable) bit))
                  editable))
              (>= n SIXTEEN)
              (let [nodes (object-array THIRTY-TWO)
                    jdx (mask hash shift)
                    jdx-node (node-assoc-ref EMPTY-BitmapIndexedNode edit (+ FIVE shift) hash key val added-leaf)]
                (loop [i ZERO j ZERO]
                  (when (< i THIRTY-TWO)
                    (if (not= (bit-and (unsigned-bit-shift-right bitmap i) ONE) ZERO)
                      (do
                        (if (nil? (aget arr j))
                          (aset nodes i (aget arr (inc j)))
                          (aset nodes i (node-assoc-ref EMPTY-BitmapIndexedNode
                                                              edit
                                                              (+ FIVE shift)
                                                              (clojure.next/hash (aget arr j))
                                                              (aget arr j)
                                                              (aget arr (inc j))
                                                              added-leaf)))
                      (recur (inc i) (+ j TWO)))
                    (recur (inc i) j))))
                (new-array-node edit (inc n) nodes))
              :else
              (let [new-array (object-array (* TWO (+ FOUR n)))]
                (acopy arr ZERO new-array ZERO (* TWO idx))
                (aset new-array (* TWO idx) key)
                (set-value! added-leaf added-leaf)
                (aset new-array (inc (* TWO idx)) val)
                (acopy arr (* TWO idx) new-array (* TWO (inc idx)) (* TWO (- n idx)))
                (let [editable (ensure-editable this edit)]
                  (set-array! editable new-array)
                  (set-bitmap! editable (bit-or (get-bitmap editable) bit))
                  editable)))))))

  (node-dissoc [this shift hash key]
    (let [bit (bit-pos hash shift)]
      (if (= (bit-and bitmap bit) ZERO)
        this
        (let [idx (bit-index bitmap bit)
              key-or-nil (aget arr (* TWO idx))
              val-or-node (aget arr (inc (* TWO idx)))]
          (cond
            (nil? key-or-nil)
            (let [n (node-dissoc val-or-node (+ FIVE shift) hash key)]
              (cond
                (= n val-or-node)
                this
                (not (nil? n))
                (new-bitmap-node nil bitmap (clone-and-set! arr (inc (* TWO idx)) n))
                (= bitmap bit)
                nil
                :else
                (new-bitmap-node nil (bit-xor bitmap bit) (remove-pair arr idx))))
            (= key key-or-nil)
            (new-bitmap-node nil (bit-xor bitmap bit) (remove-pair arr idx))
            :else
            this)))))

  (node-dissoc-ref [this edit shift hash key removed-leaf]
    (let [bit (bit-pos hash shift)]
      (if (= (bit-and bitmap bit) ZERO)
        this
        (let [idx (bit-index bitmap bit)
              key-or-nil (aget arr (* TWO idx))
              val-or-node (aget arr (inc (* TWO idx)))]
          (cond
            (nil? key-or-nil)
              (let [n (node-dissoc-ref val-or-node edit (+ FIVE shift) hash key removed-leaf)]
                (cond
                  (= n val-or-node)
                    this
                  (not (nil? n))
                    (edit-and-set! this edit (* TWO idx) nil (inc (* TWO idx)) n)
                  (= bitmap bit)
                    nil
                  :else
                    (edit-and-remove-pair this edit bit idx)))
            (= key key-or-nil)
              (do
                (set-value! removed-leaf removed-leaf)
                (edit-and-remove-pair this edit bit idx))
            :else
              this)))))

  (node-seq [this]
    (new-node-seq arr)))

(defn- new-bitmap-node [edit bitmap arr]
  (BitmapIndexedNode. edit bitmap arr))

(def ^:private EMPTY-BitmapIndexedNode (new-bitmap-node nil ZERO (object-array ZERO)))

(def ^:private NOT-FOUND (empty-object))

(defn- ensure-editable [edit]
  (when (nil? (ref-get edit))
    (throw (new-illegal-access-error "Transient used after persistent! call"))))

(deftype TransientHashMap [-edit
                           ^:volatile-mutable -root
                           ^:volatile-mutable -count
                           ^:volatile-mutable -has-nil?
                           ^:volatile-mutable -nil-value
                           -leaf-flag]

  ICounted
  (-count [this]
    (ensure-editable -edit)
    -count)

  ILookup
  (-lookup [this k not-found]
    (ensure-editable -edit)
    (cond
      (nil? k)
        (if -has-nil?
          -nil-value
          not-found)
      (nil? -root)
        not-found
      :else
        (node-find -root ZERO (->bitnum (hash k)) k not-found)))

  ITransientAssociative
  (-assoc! [this k v]
    (ensure-editable -edit)
    (if (nil? k)
      (do
        (when (not= -nil-value v)
          (set! -nil-value v))
        (when (not -has-nil?)
          (set! -count (inc -count))
          (set! -has-nil? true)))
      (do
        (set-value! -leaf-flag nil)
        (let [node (if (nil? -root)
                     EMPTY-BitmapIndexedNode
                     -root)
              node (node-assoc-ref node -edit ZERO (hash k) k v -leaf-flag)]
          (when (not= node -root)
            (set! -root node))
          (when (not (nil? (get-value -leaf-flag)))
            (set! -count (inc -count))))))
    this)

  ITransientCollection
  (-conj! [this o]
    (ensure-editable -edit)
    (cond
      (instance? platform-map-entry-type o)
        (-assoc! this (key o) (val o))
      (vector? o)
        (if (= (count o) 2)
          (-assoc! this (nth o 0) (nth o 1))
          (throw (new-argument-error "Vector arg to map conj must be a pair")))
      :else
        (loop [s (seq o)]
          (if s
            (let [entry (first s)]
              (-assoc! this (key entry) (val entry))
              (recur (next s)))
            this))))

  (-persistent [this]
    (ensure-editable -edit)
    (ref-set! -edit nil)
    (new-hash-map nil -count -root -has-nil? -nil-value))

  ITransientMap
  (-dissoc! [this k]
    (ensure-editable -edit)
    (cond
      (nil? k)
        (do
          (when -has-nil?
            (set! -has-nil? false)
            (set! -nil-value nil)
            (set! -count (dec -count)))
          this)
      (nil? -root)
        this
      :else
        (do
          (set-value! -leaf-flag nil)
          (let [node (node-dissoc-ref -root -edit ZERO (hash k) k -leaf-flag)]
            (when (not= node -root)
              (set! -root node))
            (when (not (nil? (get-value -leaf-flag)))
              (set! -count (dec -count))))
          this))))

(defn make-transient-hash-map [root count has-nil? nil-value]
  (TransientHashMap. (new-atomic-ref (thread-reference)) root count has-nil? nil-value (BoxedValue. nil)))

(declare EMPTY-HASH-MAP)

(deftype PersistentHashMap [-meta -count -root -has-nil? -nil-value]
  IAssociative
  (-assoc [this key val]
    (if (nil? key)
      (if (and -has-nil? (= val -nil-value))
        this
        (new-hash-map -meta (if -has-nil? -count (inc -count)) -root true val))
      (let [added-leaf (new-box nil)
            new-root (node-assoc (if -root -root EMPTY-BitmapIndexedNode)
                                 ZERO (->bitnum (hash key)) key val added-leaf)]
        (if (= -root new-root) ; should use identical? probably
          this
          (new-hash-map -meta
                        (if (nil? (get-value added-leaf)) -count (inc -count))
                        new-root
                        -has-nil?
                        -nil-value)))))

  (-contains-key? [this k]
    (if (nil? k)
      -has-nil?
      (if (nil? -root)
        false
        (not (identical? NOT-FOUND (node-find -root ZERO (->bitnum (hash k)) k NOT-FOUND))))))

  ICounted
  (-count [this] -count)

  IEditableCollection
  (-as-transient [this]
    (make-transient-hash-map -root -count -has-nil? -nil-value))

  IFn
  (-invoke [this k]
    (-lookup this k nil))

  (-invoke [this k not-found]
    (-lookup this k not-found))

  ILookup
  (-lookup [this key not-found]
    (cond
      (nil? key)
      (if -has-nil? -nil-value not-found)
      -root
      (node-find -root ZERO (->bitnum (hash key)) key not-found)
      :else
      not-found))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this new-meta]
    (new-hash-map new-meta count -root -has-nil? -nil-value))

  IPersistentCollection
  (-cons [this o]
    (map-cons this o))

  (-empty [this]
    (with-meta EMPTY-HASH-MAP -meta))

  IPersistentMap
  (-dissoc [this key]
    (if (nil? key)
      (if -has-nil?
        (new-hash-map -meta (dec -count) -root false nil)
        this)
      (if (nil? -root)
        this
        (let [new-root (node-dissoc -root ZERO (->bitnum (hash key)) key)]
          (if (= -root new-root)
            this
            (new-hash-map -meta (dec -count) new-root -has-nil? -nil-value))))))

  ISeqable
  (-seq [this]
    (if (nil? -root)
      nil
      (node-seq -root))
    ;return hasNull ? new Cons(new MapEntry(null, nullValue), s) : s;
    )

  obj/base-object
  (equiv/equals-method [this other]
    (map-equals? this other))

  (hash-code/hash-method [this]
    (map-hash this))

  enum/base-enumerator
  (enum/enumerable-method [this]
    (enum/new-seq-iterator (seq this))))

(defn new-hash-map [-meta -count -root -has-nil? -nil-value]
  (PersistentHashMap. -meta -count -root -has-nil? -nil-value))

(def EMPTY-HASH-MAP (new-hash-map nil ZERO nil false nil))

