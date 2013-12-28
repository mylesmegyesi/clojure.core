
; jvm platform methods

(ns clojure.lang.platform
  (:refer-clojure :only [defn defprotocol list])
  (:import [clojure.lang Util]))

(defn instance? [cls obj]
  (.isInstance cls obj))

(defn identical? [x y]
  (Util/identical x y))

(defn equals [x y]
  (Util/equiv x y))

(defn type [obj]
  (.getClass obj))

(defn platform-symbol-methods []
  ['Object
   (list 'toString ['this] 'str)
   (list 'hashCode ['this] 'hash)
   (list 'equals ['this 'other] (list 'equals 'this 'other))
   'Comparable
   (list 'compareTo ['this 'other] (list 'compare 'this 'other))])

(defn symbol-hash-code [ns name]
  (Util/hashCombine (.hashCode name) (Util/hash ns)))

(defn platform-keyword-methods []
  ['Object
   (list 'toString ['this] 'str)
   (list 'hashCode ['this] 'hash)
   (list 'equals   ['this 'other]
         (list '== 'sym 'other))

   'Comparable
   (list 'compareTo ['this 'other]
         (list 'compare 'sym 'other))])
