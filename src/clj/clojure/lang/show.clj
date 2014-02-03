(ns clojure.lang.show
  (:refer-clojure :only [defn nil? cons])
  (:require [clojure.lang.protocols     :refer [-show]]
            [clojure.lang.platform.show :refer [build-string]]))

(defn str
  ([] "")
  ([x]
   (if (nil? x) "" (-show x)))
  ([x & more]
   (build-string (cons x more))))
