(ns clojure.lang.platform.object
  (:refer-clojure :only [defn reduce concat cons fn let key val])
  (:import [clojure.lang Util]))

(defn type [obj]
  (.GetType obj))

(defn instance? [cls obj]
  (.IsAssignableFrom cls (type obj)))

(defn identical? [x y]
  (Object/ReferenceEquals x y))

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
