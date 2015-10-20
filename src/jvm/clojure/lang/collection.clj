(ns clojure.lang.collection
  (:refer-clojure :only [defmacro defn list])
  (:require [clojure.next :refer :all])
  (:import [java.util Collection]))

(defmacro import-collection-type []
  (list 'clojure.core/import '[java.util Collection]))

(def base-collection Collection)

(defn is-empty? [^Collection c]
  (.isEmpty c))

(defn contains? [^Collection c o]
  (.contains c o))

(defmacro add-method [bindings & body]
  `(add ~bindings ~@body))

(defmacro add-all-method [bindings & body]
  `(addAll ~bindings ~@body))

(defmacro clear-method [bindings & body]
  `(clear ~bindings ~@body))

(defmacro contains?-method [bindings & body]
  `(contains ~bindings ~@body))

(defmacro contains-all?-method [bindings & body]
  `(containsAll ~bindings ~@body))

(defmacro is-empty?-method [bindings & body]
  `(isEmpty ~bindings ~@body))

(defmacro remove-method [bindings & body]
  `(remove ~bindings ~@body))

(defmacro remove-all-method [bindings & body]
  `(removeAll ~bindings ~@body))

(defmacro retain-all-method [bindings & body]
  `(retainAll ~bindings ~@body))

(defmacro size-method [bindings & body]
  `(size ~bindings ~@body))

(defmacro to-array-method [bindings & body]
  `(toArray ~bindings ~@body))
