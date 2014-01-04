(ns clojure.lang.atom
  (:refer-clojure :only [apply defn defn- deftype let into])
  (:require [clojure.lang.iatom                   :refer [IAtom -compare-and-set! -reset! -swap!]]
            [clojure.lang.ideref                  :refer [IDeref]]
            [clojure.lang.equivalence             :refer [=]]
            [clojure.lang.platform.mutable-entity :as    ent]))

(defn compare-and-set! [atm old-state new-state]
  (-compare-and-set! atm old-state new-state))

(defn reset! [atm new-state]
  (-reset! atm new-state))

(defn swap!
  ([atm f] (-swap! atm f []))
  ([atm f arg] (-swap! atm f [arg]))
  ([atm f arg1 arg2] (-swap! atm f [arg1 arg2]))
  ([atm f arg1 arg2 & args] (-swap! atm f (into [arg1 arg2] args))))

(deftype Atom [-state]
  IDeref
  (-deref [this] (ent/get-entity -state))

  IAtom
  (-compare-and-set! [this old-state new-state]
    (if (= (ent/get-entity -state) old-state)
      (do
        (ent/set-entity! -state new-state)
        true)
      false))

  (-reset! [this new-state]
    (do
      (ent/set-entity! -state new-state)
      new-state))

  (-swap! [this f args]
    (let [entity (ent/get-entity -state)
          arg-list (into [entity] args)
          updated-entity (apply f arg-list)]
      (ent/set-entity! -state updated-entity)
      updated-entity))

  )

(defn atom [state]
  (Atom. (ent/make-mutable-entity state)))
