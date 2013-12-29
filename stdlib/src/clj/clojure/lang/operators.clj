(ns clojure.lang.operators
  (:refer-clojure :only [defmacro defn defn- nil? let cond and or complement prn])
  (:require [clojure.lang.platform.object :refer [equals type]]))

(defmacro when-not-nil [x y & body]
  `(let [x-nil?# (nil? ~x)
         y-nil?# (nil? ~y)]
     (cond
       (and x-nil?# y-nil?#)
       true
       (or x-nil?# y-nil?#)
       false
       :else
       ~@body)))

(defn- values-equal? [x y]
  (when-not-nil x y
    (equals x y)))

(defn- values-and-types-equal? [x y]
  (when-not-nil x y
    (and (equals (type x) (type y))
         (equals x y))))

(defn =
  "Loose eqaulity. Equality is determined by value."
  ([x y]
   (values-equal? x y)))

(def == =)

(defn ===
  "Strict eqaulity. Equality is determined by value and type."
  ([x y]
   (values-and-types-equal? x y)))

(def not= (complement =))
(def not== (complement ==))
(def not=== (complement ===))

(defn not [x]
  (if x false true))
