(ns clojure.lang.platform.symbol
  (:refer-clojure :only [defn list])
  (:import [clojure.lang Util]))

(defn platform-symbol-methods []
  ['Object
   (list 'toString ['this] 'str)
   (list 'hashCode ['this] 'hash)
   (list 'equals ['this 'other] (list 'equals 'this 'other))

   'Comparable
   (list 'compareTo ['this 'other] (list 'compare 'this 'other))])

(defn symbol-hash-code [ns name]
  (Util/hashCombine (.hashCode name) (Util/hash ns)))
