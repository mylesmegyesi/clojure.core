(ns clojure.lang.platform.hash
  (:refer-clojure :only [extend-protocol fn defn list])
  (:require [clojure.lang.hash :refer [Hash]]))

(defn platform-hash-method []
  ['Object
   (list 'hashCode ['this]
         (list 'hash 'this))])

(extend-protocol Hash
  Object
  (hash [this]
    (.hashCode this)))
