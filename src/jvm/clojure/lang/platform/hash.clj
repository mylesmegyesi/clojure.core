(ns clojure.lang.platform.hash
  (:refer-clojure :only [extend-protocol fn defn list])
  (:require [clojure.lang.ihash :refer [IHash]]))

(defn platform-hash-method []
  ['Object
   (list 'hashCode ['this]
         (list 'clojure.lang.ihash/-hash 'this))])

(extend-protocol IHash
  Object
  (hash [this]
    (.hashCode this)))
