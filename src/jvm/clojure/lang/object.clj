(ns clojure.lang.object
  (:refer-clojure :only [defmacro])
  (:import [clojure.lang.platform Identity]))

(def base-object Object)

(defmacro instance? [x y]
  `(.isInstance ~x ~y))

(defmacro identical? [x y]
  `(Identity/areIdentical ~x ~y))

(defmacro type [x]
  `(.getClass ~x))
