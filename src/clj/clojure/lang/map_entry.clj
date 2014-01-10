(ns clojure.lang.map-entry
  (:refer-clojure :refer [deftype defmacro defn defn- let and list list* -> satisfies?])
  (:require [clojure.lang.imap-entry          :refer [IMapEntry -key -val]]
            [clojure.lang.operators           :refer [=]]
            [clojure.lang.platform.comparison :refer [platform-equals-method]]
            [clojure.lang.platform.object     :refer [expand-methods]]))

(defn key [entry]
  (-key entry))

(defn val [entry]
  (-val entry))

(defmacro map-entry-equals?
  {:private true}
  [k v other]
  `(let [other# ~other]
     (and (satisfies? IMapEntry other#)
          (= ~k (key other#))
          (= ~v (val other#)))))

(defmacro map-entry-equals?-init
  {:private true}
  [this-arg other-arg]
  (list 'map-entry-equals? 'k 'v other-arg))

(def platform-map-entry-methods
  (-> {}
    (platform-equals-method 'map-entry-equals?-init)
    expand-methods))

(defmacro defmapentry [type]
  (list*
    'deftype type ['k 'v]

    'IMapEntry
    (list '-key ['this] 'k)
    (list '-val ['this] 'v)

  platform-map-entry-methods))

(defmapentry MapEntry)

(defn make-map-entry [k v]
  (MapEntry. k v))
