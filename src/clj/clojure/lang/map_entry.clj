(ns clojure.lang.map-entry
  (:refer-clojure :refer [deftype defmacro defn defn- and list list* ->])
  (:require [clojure.lang.equivalence          :refer [=]]
            [clojure.lang.iequivalence         :refer [IEquivalence]]
            [clojure.lang.imap-entry           :refer [IMapEntry -key -val]]
            [clojure.lang.platform.equivalence :refer [platform-equals-method]]
            [clojure.lang.platform.object      :refer [expand-methods]]))

(defn key [entry]
  (-key entry))

(defn val [entry]
  (-val entry))

(defn- map-entry-equals? [-key -val other]
  (and (= -key (key other))
       (= -val (val other))))

(def platform-map-entry-methods
  (-> {}
    platform-equals-method
    expand-methods))

(defmacro defmapentry [type]
  (list*
    'deftype type ['k 'v]

    'IMapEntry
    (list '-key ['this] 'k)
    (list '-val ['this] 'v)

    'IEquivalence
    (list '-equivalent? ['this 'other]
      (list 'map-entry-equals? 'k 'v 'other))

  platform-map-entry-methods))

(defmapentry MapEntry)

(defn make-map-entry [k v]
  (MapEntry. k v))
