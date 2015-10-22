(ns clojure.lang.aseq
  (:refer-clojure :only [defn defmacro when loop cond concat list list* let ->])
  (:require [clojure.lang.protocols :refer [-seq -first -next]]
            [clojure.lang.array     :as    arr]
            [clojure.next           :refer :all]))

(defn ^:private fill-array [arr sq]
  (loop [s sq i 0]
    (if s
      (do
        (aset arr i (first s))
        (recur (next s) (inc i)))
      arr)))

(defn seq->array
  ([s]
    (object-array s))
  ([s arr]
    (let [len (count s)]
      (if (> len (alength arr))
        (let [new-arr (make-array (arr/get-array-type arr) len)]
          (fill-array new-arr s)
          new-arr)
        (let [new-arr (aclone arr)]
          (fill-array new-arr s)
          (aset new-arr len nil)
          new-arr)))))

(defn seq-equal? [x y]
  (boolean
    (when (sequential? y)
      (loop [xs x ys (-seq y)]
        (cond
          (nil? xs)
            (nil? ys)
          (nil? ys)
            false
          (= (-first xs) (-first ys))
            (recur (-next xs) (-next ys))
          :else
            false)))))

(defmacro defseq [t bindings & body]
  (list* 'clojure.lang.deftype/deftype t bindings
    'clojure.lang.protocols.ISequential
    'clojure.lang.protocols.ISeqable
    (list '-seq '[this] 'this)
    'clojure.lang.object/base-object
    (list 'clojure.lang.equivalence/equals-method '[this other]
      (list 'clojure.lang.aseq/seq-equal? 'this 'other))
    body))

