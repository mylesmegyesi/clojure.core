(ns clojure.lang.aseq
  (:refer-clojure :only [defn defmacro -> when loop cond concat list list* let])
  (:require [clojure.lang.protocols :refer [-seq -first -next]]
            [clojure.next           :refer :all]))

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

