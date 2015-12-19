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

(defn class [^Object x]
  (if-let [cx x] (.getClass cx)))

(defn class-name [^Object x]
  (if-let [cx x] (.getSimpleName ^Class (.getClass cx))))

