(ns clojure.lang.hash
  (:refer-clojure :only [extend-protocol extend-type fn defn list update-in cons])
  (:require [clojure.lang.protocols :refer [IHash]])
  (:import [clojure.lang Util]))

(defn hash-combine [hash1 hash2]
  (Util/hashCombine hash1 hash2))

(defn platform-hash-method [methods init-macro]
  (update-in methods
             ['Object]
             (fn [old]
               (cons
                 (list 'hashCode ['this]
                       (list init-macro 'this))
                 old))))

(extend-protocol IHash
  Object
  (-hash [this]
    (.hashCode this)))

(extend-type nil
  IHash
  (-hash [this] 0))

