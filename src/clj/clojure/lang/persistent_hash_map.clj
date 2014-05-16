(ns clojure.lang.persistent-hash-map
  (:refer-clojure :only [defn defn- declare defprotocol deftype -> let when even? loop format cond nil? >= < and])
  (:require [clojure.lang.apersistent-map        :refer [defmap]]
            [clojure.lang.aseq                   :refer [defseq]]
            [clojure.lang.atomic-ref             :refer [new-atomic-ref]]
            [clojure.lang.map-entry              :refer [new-map-entry]]
            [clojure.lang.platform.exceptions    :refer [new-argument-error]]
            [clojure.lang.platform.hash-map      :refer [->bitnum empty-object bit-and bit-or + inc * - dec]]
            [clojure.lang.protocols              :refer [IAssociative ICounted ILookup
                                                         IMeta IPersistentMap ISeqable ISeq]]
            [clojure.next                        :refer :all :exclude [and bit-and bit-or + inc * - dec]]))

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
  (node-find       [this shift hash key not-found])
  (node-seq        [this])
  (set-array!      [this new-arr]))

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
    (bit-unsigned-shift-right hash shift)
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
       (let [arr (make-array FOUR)]
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
       (let [arr (make-array FOUR)]
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
        new-arr (make-array new-size)
        two*i (* TWO i)]
    (acopy arr ZERO new-arr ZERO two*i)
    (acopy arr (* TWO (inc i)) new-arr two*i (- new-size two*i))
    new-arr))

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
  )

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
          (let [new-arr (make-array (* TWO (inc count)))]
            (acopy array ZERO new-arr ZERO (* TWO count))
            (aset new-arr (* TWO count) key)
            (aset new-arr (inc (* TWO count)) val)
            (set-value! added-leaf added-leaf)
            (new-hash-collision-node edit hash (inc count) new-arr))))
      (let [new-arr (make-array TWO)]
        (aset new-arr ZERO nil)
        (aset new-arr ONE this)
        (node-assoc (new-bitmap-node nil (bit-pos hash shift) new-arr)
                    shift hash key val added-leaf))))
  )

(defn- new-hash-collision-node [edit hash count array]
  (HashCollisionNode. edit hash count array))

(defprotocol ^:private Bitmapped
  (get-bitmap  [this])
  (set-bitmap! [this new-bitmap]))

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
            new-arr (make-array arr-size)]
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
            (let [nodes (make-array THIRTY-TWO)
                  jdx (mask hash shift)
                  jdx-node (node-assoc EMPTY-BitmapIndexedNode
                                       (+ shift FIVE) hash key val added-leaf)]
              (aset nodes jdx jdx-node)
              (loop [i ZERO j ZERO]
                (when (< i THIRTY-TWO)
                  (if (not= (bit-and (bit-unsigned-shift-right bitmap i) ONE) ZERO)
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
            (let [new-arr (make-array (* TWO (inc n)))]
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
              (let [nodes (make-array THIRTY-TWO)
                    jdx (mask hash shift)
                    jdx-node (node-assoc-ref EMPTY-BitmapIndexedNode edit (+ FIVE shift) hash key val added-leaf)]
                (loop [i ZERO j ZERO]
                  (when (< i THIRTY-TWO)
                    (if (not= (bit-and (bit-unsigned-shift-right i) ONE) ZERO)
                      (do
                        (if (nil? (aget arr j))
                          (aset nodes i (aget (inc j)))
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
              (let [new-array (make-array (* TWO (+ FOUR n)))]
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

  (node-seq [this]
    (new-node-seq arr))

  )

(defn- new-bitmap-node [edit bitmap arr]
  (BitmapIndexedNode. edit bitmap arr))

(def ^:private EMPTY-BitmapIndexedNode (new-bitmap-node nil ZERO (make-array ZERO)))

(def ^:private NOT-FOUND (empty-object))

(defmap PersistentHashMap [-meta -count -root -has-nil? -nil-value]
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

  ICounted
  (-count [this] -count)

  ILookup
  (-lookup [this key not-found]
    (cond
      (nil? key)
      (if -has-nil? -nil-value not-found)
      -root
      (node-find -root ZERO (->bitnum (hash key)) key not-found)
      :else
      not-found))

  (-includes? [this key]
    (if (nil? key)
      -has-nil?
      (if (nil? -root)
        false
        (not (identical? NOT-FOUND (node-find -root ZERO (->bitnum (hash key)) key NOT-FOUND))))))

  IMeta
  (-meta [this] -meta)
  (-with-meta [this new-meta]
    (new-hash-map new-meta count -root -has-nil? -nil-value))

  IPersistentMap
  (-dissoc [this key]
    (if (nil? key)
      (if -has-nil?
        (new-hash-map -meta (dec -count) -root false nil)
        this)
      (let [new-root (node-dissoc -root ZERO (->bitnum (hash key)) key)]
        (if (= -root new-root)
          this
          (new-hash-map -meta (dec -count) new-root -has-nil? -nil-value)))))

  ISeqable
  (-seq [this]
    (if (nil? -root)
      nil
      (node-seq -root))
    ;return hasNull ? new Cons(new MapEntry(null, nullValue), s) : s;
    ))

(defn new-hash-map [-meta -count -root -has-nil? -nil-value]
  (PersistentHashMap. -meta -count -root -has-nil? -nil-value))

(def EMPTY-HASH-MAP (new-hash-map nil ZERO nil false nil))
