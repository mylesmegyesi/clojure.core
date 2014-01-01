(ns clojure.lang.platform.hash
  (:refer-clojure :only [extend-protocol fn defn list update-in cons])
  (:require [clojure.lang.ihash :refer [IHash]]))

(defn platform-hash-method [methods]
  (update-in methods
             ['Object]
             (fn [old]
               (cons
                 (list 'hashCode ['this]
                       (list 'clojure.lang.ihash/-hash 'this))
                 old))))

(extend-protocol IHash
  Object
  (-hash [this]
    (.hashCode this)))
