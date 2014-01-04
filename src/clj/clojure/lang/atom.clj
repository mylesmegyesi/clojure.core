(ns clojure.lang.atom
  (:refer-clojure :only [apply defn deftype let loop into])
  (:require [clojure.lang.iatom                  :refer [IAtom -compare-and-set! -reset! -swap!]]
            [clojure.lang.ideref                 :refer [IDeref]]
            [clojure.lang.equivalence            :refer [=]]
            [clojure.lang.platform.atomic-entity :as    ent]))

(defn compare-and-set! [atm old-state new-state]
  (-compare-and-set! atm old-state new-state))

(defn reset! [atm new-state]
  (-reset! atm new-state))

(defn swap!
  ([atm f] (-swap! atm f []))
  ([atm f x] (-swap! atm f [x]))
  ([atm f x y] (-swap! atm f [x y]))
  ([atm f x y & args] (-swap! atm f (into [x y] args))))

(deftype Atom [-state]
  IDeref
  (-deref [this] (ent/get-entity -state))

  IAtom
  (-compare-and-set! [this old-state new-state]
    (ent/compare-and-set-entity! -state old-state new-state))

  (-reset! [this new-state]
    (do
      (ent/set-entity! -state new-state)
      new-state))

  (-swap! [this f args]
    (loop []
      (let [entity (ent/get-entity -state)
            arg-list (into [entity] args)
            updated-entity (apply f arg-list)]
        (if (ent/compare-and-set-entity! -state entity updated-entity)
          updated-entity
          (recur)))))

  )

(defn atom [state]
  (Atom. (ent/make-atomic-entity state)))
