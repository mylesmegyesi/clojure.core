(ns clojure.lang.platform.hash
  (:refer-clojure :only [extend-protocol fn defn list update-in cons])
  (:require [clojure.lang.ihash :refer [IHash]]
            [clojure.lang.platform.numbers]))

(defn platform-hash-method [methods init-macro]
  (update-in methods
             ['Object]
             (fn [old]
               (cons
                 (list 'GetHashCode ['this]
                       (list init-macro 'this))
                 old))))

(extend-protocol IHash
  Object
  (-hash [this]
    (.GetHashCode this))

  )
