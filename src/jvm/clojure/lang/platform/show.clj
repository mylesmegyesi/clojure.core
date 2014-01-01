(ns clojure.lang.platform.show
  (:refer-clojure :only [defmacro extend-protocol fn defn list empty? loop first rest update-in cons])
  (:require [clojure.lang.ishow :refer [IShow -show]])
  (:import [java.lang StringBuilder]))

(defn platform-show-method [methods]
  (update-in methods
             ['Object]
             (fn [old]
               (cons
                 (list 'toString ['this]
                       (list 'clojure.lang.ishow/-show 'this))
                 old))))

(extend-protocol IShow
  Object
  (-show [this]
    (.toString this)))

(defmacro build-string [strs]
  `(loop [sb# (StringBuilder. "") strs# ~strs]
    (if (empty? strs#)
      (.toString sb#)
      (recur (.append sb# (-show (first strs#))) (rest strs#)))))
