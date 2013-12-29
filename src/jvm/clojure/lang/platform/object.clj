(ns clojure.lang.platform.object
  (:refer-clojure :only [defn])
  (:import [clojure.lang Util]))

(defn instance? [cls obj]
  (.isInstance cls obj))

(defn identical? [x y]
  (Util/identical x y))

(defn equals [x y]
  (Util/equiv x y))

(defn type [obj]
  (.getClass obj))
