(ns clojure.lang.object
  (:refer-clojure :only [defmacro defn if-let nil?])
  (:import [clojure.lang.platform Identity]))

(def base-object Object)
(def base-class Class)
(def platform-string String)

(defmacro new-base-object []
  `(Object.))

(defn instance? [^Class x y]
  (.isInstance x y))

(defmacro identical? [x y]
  `(Identity/areIdentical ~x ~y))

(defmacro type [x]
  `(if-let [x# ~x] (.getClass x#)))
