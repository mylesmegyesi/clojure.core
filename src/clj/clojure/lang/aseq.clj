(ns clojure.lang.aseq
  (:refer-clojure :only [defn defmacro -> when loop cond nil? concat list* list let])
  (:require [clojure.lang.deftype              :refer [expand-methods]]
            [clojure.lang.iseqable             :refer [-seq]]
            [clojure.lang.iseq                 :refer [-first -next]]
            [clojure.lang.operators            :refer [boolean =]]
            [clojure.lang.platform.equivalence :refer [platform-equals-method]]
            [clojure.lang.sequential           :refer [sequential?]]))

(defmacro seq-equal? [x y]
  `(boolean
     (let [y# ~y]
       (when (sequential? y#)
         (loop [xs# ~x ys# (-seq y#)]
           (cond (nil? xs#) (nil? ys#)
                 (nil? ys#) false
                 (= (-first xs#) (-first ys#))
                 (recur (-next xs#) (-next ys#))
                 :else false))))))

(defmacro defseq [type bindings & body]
  (concat
    (list* 'clojure.core/deftype type bindings
           'clojure.lang.isequential/ISequential
           'clojure.lang.iseqable/ISeqable
           (list '-seq ['this] 'this)
           body)
    (-> {}
      (platform-equals-method 'clojure.lang.aseq/seq-equal?)
      expand-methods)))
