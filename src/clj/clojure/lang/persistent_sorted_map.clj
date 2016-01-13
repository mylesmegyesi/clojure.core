(ns clojure.lang.persistent-sorted-map
  (:refer-clojure :only [cond declare defn defn- defprotocol format if-let let loop])
  (:require [clojure.lang
              [afn             :refer [deffn]]
              [apersistent-map :refer [map-cons map-equals? map-hash]]
              [aseq            :refer [defseq]]
              [deftype         :refer [deftype]]
              [enumerable      :as    enum]
              [equivalence     :as    equiv]
              [exceptions      :refer [new-argument-error new-unsupported-error]]
              [hash            :as    hash-code]
              [map-entry       :refer [new-map-entry]]
              [object          :as    obj]
              [persistent-list :refer [EMPTY-LIST]]
              [protocols       :refer [ICounted ILookup IMeta IObj IFn
                                       IAssociative IPersistentCollection IPersistentMap
                                       ISeq ISeqable ISequential ISorted
                                       -lookup]]]
            [clojure.next :refer :all]))

(declare red-node?)
(declare black-node?)

(declare make-red-node)
(declare make-sorted-red-node)
(declare make-sorted-red-branch)

(declare make-black-node)
(declare make-sorted-black-node)
(declare make-sorted-black-branch)

(declare new-sorted-map)

(declare balance-left-del)
(declare balance-right-del)

(defprotocol SortedNode ^:private
  (-color [this])
  (-entry [this])
  (-left [this])
  (-right [this])
  (-add-left [this node])
  (-add-right [this node])
  (-remove-left [this node])
  (-remove-right [this node])
  (-balance-left [this parent-node])
  (-balance-right [this parent-node])
  (-blacken [this])
  (-redden [this])
  (-replace [this entry left right]))

(deftype SortedBlackNode ^:private
  [-map-entry]

  SortedNode
  (-color [this] :black)
  (-entry [this] -map-entry)
  (-left [this] nil)
  (-right [this] nil)
  (-add-left [this node]
    (-balance-left node this))
  (-add-right [this node]
    (-balance-right node this))
  (-remove-left [this node]
    (balance-left-del -map-entry node nil))
  (-remove-right [this node]
    (balance-right-del -map-entry nil node))
  (-balance-left [this node]
    (make-black-node (-entry node) this (-right node)))
  (-balance-right [this node]
    (make-black-node (-entry node) (-left node) this))
  (-blacken [this] this)
  (-redden [this]
    (make-sorted-red-node -map-entry))
  (-replace [this entry left right]
    (make-black-node entry left right)))

(deftype SortedBlackBranch ^:private
  [-map-entry -left-node -right-node]

  SortedNode
  (-color [this] :black)
  (-entry [this] -map-entry)
  (-left [this] -left-node)
  (-right [this] -right-node)
  (-add-left [this node]
    (-balance-left node this))
  (-add-right [this node]
    (-balance-right node this))
  (-remove-left [this node]
    (balance-left-del -map-entry node -right-node))
  (-remove-right [this node]
    (balance-right-del -map-entry -left-node node))
  (-balance-left [this node]
    (make-black-node (-entry node) this (-right node)))
  (-balance-right [this node]
    (make-black-node (-entry node) (-left node) this))
  (-blacken [this] this)
  (-redden [this]
    (make-sorted-red-branch -map-entry -left-node -right-node))
  (-replace [this entry left right]
    (make-black-node entry left right)))

(deftype SortedRedNode ^:private
  [-map-entry]

  SortedNode
  (-color [this] :red)
  (-entry [this] -map-entry)
  (-left [this] nil)
  (-right [this] nil)
  (-add-left [this node]
    (make-red-node -map-entry node nil))
  (-add-right [this node]
    (make-red-node -map-entry nil node))
  (-remove-left [this node]
    (make-red-node -map-entry node nil))
  (-remove-right [this node]
    (make-red-node -map-entry nil node))
  (-balance-left [this node]
    (make-black-node (-entry node) this (-right node)))
  (-balance-right [this node]
    (make-black-node (-entry node) (-left node) this))
  (-blacken [this]
    (make-sorted-black-node -map-entry))
  (-redden [this]
    (throw (new-unsupported-error "Invariant Violation")))
  (-replace [this entry left right]
    (make-red-node entry left right)))

(deftype SortedRedBranch ^:private
  [-map-entry -left-node -right-node]

  SortedNode
  (-color [this] :red)
  (-entry [this] -map-entry)
  (-left [this] -left-node)
  (-right [this] -right-node)
  (-add-left [this node]
    (make-red-node -map-entry node -right-node))
  (-add-right [this node]
    (make-red-node -map-entry -left-node node))
  (-remove-left [this node]
    (make-red-node -map-entry node -right-node))
  (-remove-right [this node]
    (make-red-node -map-entry -left-node node))
  (-balance-left [this node]
    (cond
      (red-node? -left-node)
        (let [blackened-node (make-black-node (-entry node) -right-node (-right node))]
          (make-red-node -map-entry (-blacken -left-node) blackened-node))
      (red-node? -right-node)
        (let [blackened (make-black-node -map-entry -left-node (-left -right-node))
              blackened-right (make-black-node (-entry node) (-right -right-node) (-right node))]
          (make-red-node (-entry -right-node) blackened blackened-right))
      :else
        (make-black-node (-entry node) this (-right node))))
  (-balance-right [this node]
    (cond
      (red-node? -right-node)
        (let [blackened (make-black-node (-entry node) (-left node) -left-node)]
          (make-red-node -map-entry blackened (-blacken -right-node)))
      (red-node? -left-node)
        (let [blackened-left (make-black-node (-entry node) (-left node) (-left -left-node))
              blackened-right (make-black-node -map-entry (-right -left-node) -right-node)]
          (make-red-node (-entry -left-node) blackened-left blackened-right))
      :else
        (make-black-node (-entry node) (-left node) this)))
  (-blacken [this]
    (make-sorted-black-branch -map-entry -left-node -right-node))
  (-redden [this]
    (throw (new-unsupported-error "Invariant Violation")))
  (-replace [this entry left right]
    (make-red-node entry left right)))

(defn- red-node? [node]
  (and
    (not (nil? node))
    (= :red (-color node))))

(defn- black-node? [node]
  (and
    (not (nil? node))
    (= :black (-color node))))

(defn- make-sorted-red-node [-map-entry]
  (SortedRedNode. -map-entry))

(defn- make-sorted-red-branch [-map-entry -left -right]
  (SortedRedBranch. -map-entry -left -right))

(defn- make-sorted-black-node [-map-entry]
  (SortedBlackNode. -map-entry))

(defn- make-sorted-black-branch [-map-entry -left -right]
  (SortedBlackBranch. -map-entry -left -right))

(defn- make-black-node [-map-entry left right]
  (if (and (nil? left) (nil? right))
    (make-sorted-black-node -map-entry)
    (make-sorted-black-branch -map-entry left right)))

(defn- make-red-node [-map-entry left right]
  (if (and (nil? left) (nil? right))
    (make-sorted-red-node -map-entry)
    (make-sorted-red-branch -map-entry left right)))

(defn- left-balance [-map-entry ins right]
  (cond
    (and (red-node? ins) (red-node? (-left ins)))
      (let [blackened (make-black-node -map-entry (-right ins) right)]
        (make-red-node (-entry ins) (-blacken (-left ins)) blackened))
    (and (red-node? ins) (red-node? (-right ins)))
      (let [blackened-ins (make-black-node (-entry ins) (-left ins) (-left (-right ins)))
            blackened (make-black-node -map-entry (-right (-right ins)) right)]
        (make-red-node (-entry (-right ins)) blackened-ins blackened))
    :else
      (make-black-node -map-entry ins right)))

(defn- right-balance [-map-entry left ins]
  (cond
    (and (red-node? ins) (red-node? (-right ins)))
      (let [blackened (make-black-node -map-entry left (-left ins))]
        (make-red-node (-entry ins) blackened (-blacken (-right ins))))
    (and (red-node? ins) (red-node? (-left ins)))
      (let [blackened (make-black-node -map-entry left (-left (-left ins)))
            blackened-ins (make-black-node (-entry ins) (-right (-left ins)) (-right ins))]
        (make-red-node (-entry (-left ins)) blackened blackened-ins))
    :else
      (make-black-node -map-entry left ins)))

(defn- balance-left-del [-map-entry del right]
  (cond
    (red-node? del)
      (make-red-node -map-entry (-blacken del) right)
    (black-node? right)
      (right-balance -map-entry del (-redden right))
    (and (red-node? right) (black-node? (-left right)))
      (let [blackened (make-black-node -map-entry del (-left (-left right)))
            r-balance (right-balance (-entry right) (-right (-left right)) (-redden (-right right)))]
        (make-red-node (-entry (-left right)) blackened r-balance))
    :else
      (throw (new-unsupported-error "Invariant Violation"))))

(defn- balance-right-del [-map-entry left del]
  (cond
    (red-node? del)
      (make-red-node -map-entry left (-blacken del))
    (black-node? left)
      (left-balance -map-entry (-redden left) del)
    (and (red-node? left) (black-node? (-right left)))
      (let [l-balance (left-balance (-entry left) (-redden (-left left)) (-left (-right left)))
            blackened (make-black-node -map-entry (-right (-right left)) del)]
        (make-red-node (-entry (-right left)) l-balance blackened))
    :else
      (throw (new-unsupported-error "Invariant Violation"))))

(defn- sorted-map-add [root compare-fn k v]
  (if (nil? root)
    [(make-sorted-red-node (new-map-entry k v)) false]
    (let [comparison (compare-fn k (key (-entry root)))]
      (if (zero? comparison)
        [root true]
        (let [[node existed?] (if (< comparison 0)
                                (sorted-map-add (-left root) compare-fn k v)
                                (sorted-map-add (-right root) compare-fn k v))]
          (if existed?
            [node existed?]
            (if (< comparison 0)
              [(-add-left root node) false]
              [(-add-right root node) false])))))))

(defn- sorted-map-append [left right]
  (cond
    (nil? left)
      right
    (nil? right)
      left
    (red-node? left)
      (if (red-node? right)
        (let [app (sorted-map-append (-right left) (-left right))]
          (if (red-node? app)
            (let [redened-left (make-red-node (-entry left) (-left left) (-left app))
                  redened-right (make-red-node (-entry right) (-right app) (-right right))]
              (make-red-node (-entry app) redened-left redened-right))
            (let [redened (make-red-node (-entry right) app (-right right))]
              (make-red-node (-entry left) (-left left) redened))))
        (make-red-node (-entry left) (-left left) (sorted-map-append (-right left) right)))
    (red-node? right)
      (make-red-node (-entry right) (sorted-map-append left (-left right)) (-right right))
    :else
      (let [app (sorted-map-append (-right left) (-left right))]
        (if (red-node? app)
          (let [blackened-left (make-black-node (-entry left) (-left left) (-left app))
                blackened-right (make-black-node (-entry right) (-right app) (-right right))]
              (make-red-node (-entry app) blackened-left blackened-right))
          (let [blackened (make-black-node (-entry right) app (-right right))]
            (balance-left-del (-entry left) (-left left) blackened))))))

(defn- sorted-map-remove [root compare-fn k]
  (if (nil? root)
    [root false]
    (let [comparison (compare-fn k (key (-entry root)))]
      (if (zero? comparison)
        [(sorted-map-append (-left root) (-right root)) true]
        (let [[node found?] (if (< comparison 0)
                              (sorted-map-remove (-left root) compare-fn k)
                              (sorted-map-remove (-right root) compare-fn k))]
          (if (not found?)
            [root false]
            (if (< comparison 0)
              (if (black-node? (-left root))
                [(balance-left-del (-entry root) node (-right root)) true]
                [(make-red-node (-entry root) node (-right root)) true])
              (if (black-node? (-right root))
                [(balance-right-del (-entry root) (-left root) node) true]
                [(make-red-node (-entry root) (-left root) node) true]))))))))

(defn- sorted-map-replace [root compare-fn k v]
  (let [comparison (compare-fn k (key (-entry root)))
         new-val (if (zero? comparison)
                   v
                   (val (-entry root)))
         left (if (< comparison 0)
                (sorted-map-replace (-left root) compare-fn k v)
                (-left root))
         right (if (> comparison 0)
                 (sorted-map-replace (-right root) compare-fn k v)
                 (-right root))]
    (-replace root (new-map-entry (key (-entry root)) new-val) left right)))

(defn- sorted-map-assoc [root compare-fn k v]
  (let [[node existed?] (sorted-map-add root compare-fn k v)]
    (if existed?
      (if (= v (val (-entry node)))
        [root 0]
        [(sorted-map-replace root compare-fn k v) 0])
      [(-blacken node) 1])))

(defn- sorted-map-dissoc [root compare-fn k]
  (let [[node found?] (sorted-map-remove root compare-fn k)]
    (if found?
      [node 1]
      [root 0])))

(defn- sorted-map-includes? [node compare-fn k]
  (if (nil? node)
    false
    (let [comparison (compare-fn k (key (-entry node)))]
      (cond
        (zero? comparison)
          true
        (< comparison 0)
          (recur (-left node) compare-fn k)
        (> comparison 0)
          (recur (-right node) compare-fn k)))))

(defn- sorted-map-lookup [-root compare-fn k default]
  (loop [node -root
         found false]
    (cond
      found
        (val (-entry node))
      (nil? node)
        default
      :else
        (let [comparison (compare-fn k (key (-entry node)))]
          (cond
            (zero? comparison)
              (recur node true)
            (< comparison 0)
              (recur (-left node) false)
            (> comparison 0)
              (recur (-right node) false))))))

(declare make-seq-stack)
(declare make-sorted-map-seq)

(defseq PersistentSortedMapSeq [-stack -count]
  ICounted
  (-count [this] -count)

  ISeq
  (-first [this]
    (-entry (first -stack)))

  (-next [this]
    (let [node (first -stack)
          next-stack (make-seq-stack (-right node) (next -stack))]
      (make-sorted-map-seq next-stack (dec -count))))

  (-more [this]
    (if-let [s (next this)] s EMPTY-LIST))

  )

(defn- make-seq-stack [tree stack]
  (if (nil? tree)
    stack
    (recur (-left tree) (cons tree stack))))

(defn- make-sorted-map-seq [stack cnt]
  (if (nil? (first stack))
    nil
    (PersistentSortedMapSeq. stack cnt)))

(declare EMPTY-SORTED-MAP)

(deffn PersistentTreeMap [-root -count -comparator -meta] ; PersistentTreeMap is the clojure class name
  IAssociative
  (-assoc [this k v]
    (let [[tree cnt] (sorted-map-assoc -root -comparator k v)]
      (if (= tree -root)
        this
        (new-sorted-map tree (+ -count cnt) -comparator -meta))))

  (-contains-key? [this k]
    (sorted-map-includes? -root -comparator k))

  ICounted
  (-count [this] -count)

  IFn
  (-invoke [this k]
    (-lookup this k nil))

  (-invoke [this k not-found]
    (-lookup this k not-found))

  ILookup
  (-lookup [this k default]
    (sorted-map-lookup -root -comparator k default))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (new-sorted-map -root -count -comparator m))

  IPersistentCollection
  (-cons [this o]
    (map-cons this o))

  (-empty [this]
    (with-meta EMPTY-SORTED-MAP -meta))

  IPersistentMap
  (-dissoc [this k]
    (let [[tree cnt] (sorted-map-dissoc -root -comparator k)]
      (if (= tree -root)
        this
        (new-sorted-map tree (- -count cnt) -comparator -meta))))

  ISeqable
  (-seq [this]
    (make-sorted-map-seq (make-seq-stack -root nil) -count))

  obj/base-object
  (equiv/equals-method [this other]
    (map-equals? this other))

  (hash-code/hash-method [this]
    (map-hash this))

  enum/base-enumerator
  (enum/enumerable-method [this]
    (enum/new-seq-iterator (seq this)))

  ISorted)

(defn- new-sorted-map [root cnt compare-fn mta]
  (PersistentTreeMap. root cnt compare-fn mta))

(def EMPTY-SORTED-MAP (new-sorted-map nil 0 compare nil))

(defn make-sorted-map [compare-fn args]
  (let [arg-count (count args)]
    (if (even? arg-count)
      (let [empty-sorted-map (new-sorted-map nil 0 compare-fn nil)]
        (loop [kvs args
               -sorted-map empty-sorted-map]
          (if (empty? kvs)
            -sorted-map
            (let [k (first kvs)
                  v (second kvs)]
              (recur
                (rest (next kvs))
                (assoc -sorted-map k v))))))
      (throw (new-argument-error
               (format "PersistentTreeMap can only be created with even number of arguments: %s arguments given"
                arg-count))))))

