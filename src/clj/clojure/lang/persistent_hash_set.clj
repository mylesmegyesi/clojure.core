(ns clojure.lang.persistent-hash-set
  (:refer-clojure :only [apply assoc cons declare defmacro defn defn- deftype dissoc empty? every? first fn flatten hash-map keys let list list* loop map next reduce repeat rest take + ->])
  (:require [clojure.lang.icounted            :refer [ICounted]]
            [clojure.lang.ilookup             :refer [ILookup]]
            [clojure.lang.ipersistent-set     :refer [IPersistentSet]]
            [clojure.lang.iseqable            :refer [ISeqable]]
            [clojure.lang.comparison          :refer [=]]
            [clojure.lang.counted             :refer [count]]
            [clojure.lang.hash                :refer [hash]]
            [clojure.lang.lookup              :refer [contains?]]
            [clojure.lang.seq                 :refer [seq]]
            [clojure.lang.platform.comparison :refer [platform-equals-method]]
            [clojure.lang.platform.hash       :refer [platform-hash-method]]
            [clojure.lang.platform.object     :refer [expand-methods]]))

(defn- make-pairs [xs]
  (flatten (map #(take 2 (repeat %)) xs)))

(defn- sets-reduce [accumulator-fn -hash-map sets]
  (loop [remaining-sets sets
         new-hash-map   -hash-map]
    (if (empty? remaining-sets)
      new-hash-map
      (recur (rest remaining-sets)
             (accumulator-fn new-hash-map (seq (first remaining-sets)))))))

(defn- hash-set-difference [-hash-map sets]
  (sets-reduce #(apply dissoc (cons %1 %2)) -hash-map sets))

(defn- hash-set-union [-hash-map sets]
  (sets-reduce #(apply assoc (cons %1 (make-pairs %2))) -hash-map sets))

(defn- hash-set-intersection [test-items sets]
  (reduce
    (fn [acc x]
      (if (every? #(contains? % x) sets)
          (assoc acc x x)
          acc))
    (hash-map) test-items))

(defn- hash-set-hash [items-seq]
  (reduce #(+ %1 (hash %2)) 0 items-seq))

(defn- hash-set-equals? [-hash-map other-set]
  (let [ks (keys -hash-map)]
    (if (= (clojure.core/count ks) (count other-set))
        (every? #(contains? other-set %) ks)
        false)))

(defmacro hash-set-hash-init
  {:private true}
  [this]
  (list 'hash-set-hash (list 'seq this)))

(defmacro hash-set-equals?-init
  {:private true}
  [this other-set]
  (list 'hash-set-equals? '-hash-map other-set))

(declare make-hash-set)

(def platform-hash-set-methods
  ^{:private true}
  (-> {}
    (platform-hash-method 'hash-set-hash-init)
    (platform-equals-method 'hash-set-equals?-init)
    expand-methods))

(defmacro defpersistenthashset [type]
  (list*
    'deftype type ['-hash-map]

    'ICounted
    (list '-count ['this]
      (list 'clojure.core/count (list 'keys '-hash-map)))

    'ILookup
    (list '-lookup ['this 'x 'default]
      (list 'clojure.core/get '-hash-map 'x 'default))

    (list '-includes? ['this 'x]
      (list 'clojure.core/contains? '-hash-map 'x))

    'IPersistentSet
    (list '-conj ['this 'xs]
      (list 'let ['new-hash-map (list 'apply 'assoc '-hash-map (list 'make-pairs 'xs))]
        (list 'make-hash-set 'new-hash-map)))

    (list '-difference ['this 'sets]
      (list 'let ['new-hash-map (list 'hash-set-difference '-hash-map 'sets)]
        (list 'make-hash-set 'new-hash-map)))

    (list '-disj ['this 'xs]
      (list 'let ['new-hash-map (list 'apply 'dissoc '-hash-map 'xs)]
        (list 'make-hash-set 'new-hash-map)))

    (list '-intersection ['this 'sets]
      (list 'let ['new-hash-map (list 'hash-set-intersection (list 'keys '-hash-map) 'sets)]
        (list 'make-hash-set 'new-hash-map)))

    (list '-union ['this 'sets]
      (list 'let ['new-hash-map (list 'hash-set-union '-hash-map 'sets)]
        (list 'make-hash-set 'new-hash-map)))

    'ISeqable
    (list '-seq ['this]
      (list 'clojure.core/seq (list 'keys '-hash-map)))

    platform-hash-set-methods))

(defpersistenthashset PersistentHashSet)

(defn- make-hash-set [-hash-map]
  (PersistentHashSet. -hash-map))

(defn hash-set
  ([] (make-hash-set (hash-map)))
  ([& xs]
    (let [pairs (make-pairs xs)]
      (make-hash-set (apply hash-map pairs)))))
