(ns clojure.lang.platform.object
  (:refer-clojure :only [defn reduce concat cons fn let key val])
  (:import [clojure.lang Util]))

(defn instance? [cls obj]
  (.isInstance cls obj))

(defn identical? [x y]
  (Util/identical x y))

(defn equals [x y]
  (Util/equiv x y))

(defn type [obj]
  (.getClass obj))

(defn hash-combine [hash1 hash2]
  (Util/hashCombine hash1 hash2))

(defn expand-methods [methods]
  (reduce
    (fn [acc entry]
      (let [protocol (key entry)]
        (concat (cons protocol (val entry))
                acc)))
    []
    methods))
