(ns clojure.lang.map-entry
  (:refer-clojure :refer [deftype defmacro defn defn- let list list* -> satisfies?])
  (:require [clojure.lang.deftype     :refer [expand-methods]]
            [clojure.lang.equivalence :refer [platform-equals-method]]
            [clojure.lang.protocols   :refer [IMapEntry]]
            [clojure.next             :refer :all]))

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

(defmacro defmapentry [type]
  (list*
    'deftype type ['k 'v]

    'IMapEntry
    (list '-key ['this] 'k)
    (list '-val ['this] 'v)

    (-> {}
      (platform-equals-method 'map-entry-equals?-init)
      expand-methods)))

(defmapentry MapEntry)

(defn new-map-entry [k v]
  (MapEntry. k v))
