(ns clojure.lang.apersistent-vector
  (:refer-clojure :only [defmacro defn def])
  (:require [clojure.lang.deftype              :refer [expand-methods]]
            [clojure.lang.platform.equivalence :refer [platform-equals-method]]
            [clojure.lang.platform.hash        :refer [platform-hash-method]]))

(def platform-set-methods
  (-> {}
    (platform-equals-method 'clojure.lang.apersistent-vector/vector-equals?-init)
    (platform-hash-method 'clojure.lang.apersistent-vector/vector-hash-init)
    platform-enumerable-method
    platform-list-method
    platform-random-access-method
    platform-serializable-method
    expand-methods))

(defmacro defvector [type gen-next]
  (list*
    'clojure.core/deftype type ['-vec]

    'clojure.lang.ipersistent-vector/IPersistentVector
    (list '-assoc-n ['this 'n 'x])

    (list '-length ['this]
      (list '-count 'this))

    (list '-cons ['this 'x])

    'clojure.lang.associative/Associative
    (list '-contains-key ['this 'key])

    (list '-entry-at ['this 'key])

    (list '-assoc ['this 'key 'val])

    'clojure.lang.ipersistent-collection/IPersistentCollection
    (list '-count ['this])

    (list '-empty ['this])

    (list '-equiv ['this])

    'clojure.lang.seqable/Seqable
    (list '-seq ['this])

    'clojure.lang.ilookup/ILookup
    (list '-val-at ['this 'key])

    (list '-val-at ['this 'key 'not-found])

    'clojure.lang.ipersistent-stack/IPersistentStack
    (list '-peek ['this])

    (list '-pop ['this])

    'clojure.lang.reversible/Reversible
    (list '-rseq ['this])

    'clojure.lang.indexed/Indexed
    (list '-nth ['this 'n])

    (list '-nth ['this 'n 'not-found])

    'clojure.lang.counted/Counted
    (list '-count ['this])

    'clojure.lang.ihash-eq/IHashEq
    (list '-hasheq ['this])

    clojure.lang.apersistent-set/platform-set-methods))
