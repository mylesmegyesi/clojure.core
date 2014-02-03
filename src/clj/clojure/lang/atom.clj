(ns clojure.lang.atom
  (:refer-clojure :only [apply assoc cons defn defn- deftype dissoc empty? first fn let loop if-not into rest second])
  (:require [clojure.lang.persistent-array-map   :refer [array-map]]
            [clojure.lang.platform.atomic-entity :as    ent]
            [clojure.lang.platform.exceptions    :refer [new-illegal-state-error]]
            [clojure.lang.protocols              :refer [IAtom -compare-and-set! -reset! -swap!
                                                         IDeref IMeta IValidatable IWatchable]]
            [clojure.next                        :refer :all :exclude [first empty?]]))

(defn compare-and-set! [atm old-val new-val]
  (-compare-and-set! atm old-val new-val))

(defn reset! [atm new-val]
  (-reset! atm new-val))

(defn swap!
  ([atm f] (-swap! atm f []))
  ([atm f x] (-swap! atm f [x]))
  ([atm f x y] (-swap! atm f [x y]))
  ([atm f x y & args] (-swap! atm f (into [x y] args))))

(defn- validate-with-exception [validator-fn input]
  (if validator-fn
    (if-not (validator-fn input)
      (throw (new-illegal-state-error "Invalid reference state")))))

(defn- notify-watches [watches-map atm old-value new-value]
  (loop [watches watches-map]
    (if-not (empty? watches)
      (let [watch (first watches)
            watch-key (first watch)
            watch-fn (second watch)]
        (watch-fn watch-key atm old-value new-value)
        (recur (rest watches))))))

(deftype Atom [-state -meta -validator -watches]
  IDeref
  (-deref [this] (ent/get-entity -state))

  IMeta
  (-meta [this] (ent/get-entity -meta))

  (-reset-meta! [this new-meta]
    (do
      (ent/set-entity! -meta new-meta)
      new-meta))

  (-alter-meta! [this f args]
    (let [meta-args (cons (ent/get-entity -meta) args)
          new-meta (apply f meta-args)]
      (ent/set-entity! -meta new-meta)
      new-meta))

  IAtom
  (-compare-and-set! [this old-state new-state]
    (do
      (validate-with-exception (ent/get-entity -validator) new-state)
      (if (ent/compare-and-set-entity! -state old-state new-state)
        (do
          (notify-watches (ent/get-entity -watches) this old-state new-state)
          true)
        false)))

  (-reset! [this new-state]
    (let [old-state (ent/get-entity -state)
          self this]
      (validate-with-exception (ent/get-entity -validator) new-state)
      (ent/set-entity! -state new-state)
      (notify-watches (ent/get-entity -watches) self old-state new-state)
      new-state))

  (-swap! [this f args]
    (loop []
      (let [entity (ent/get-entity -state)
            arg-list (cons entity args)
            updated-entity (apply f arg-list)
            self this]
        (validate-with-exception (ent/get-entity -validator) updated-entity)
        (if (ent/compare-and-set-entity! -state entity updated-entity)
          (do
            (notify-watches (ent/get-entity -watches) self entity updated-entity)
            updated-entity)
          (recur)))))

  IValidatable
  (-get-validator [this] (ent/get-entity -validator))

  (-set-validator! [this f] (ent/set-entity! -validator f))

  IWatchable
  (-add-watch [this k f]
    (do
      (ent/set-entity! -watches
        (assoc (ent/get-entity -watches) k f))
      this))

  (-remove-watch [this k]
    (let [watches (ent/get-entity -watches)
          new-watches (dissoc watches k)
          self this]
      (ent/set-entity! -watches new-watches)
      self)))

(def ^{:private true} default-meta nil)
(def ^{:private true} default-validator nil)

(defn atom
  ([state]
    (atom state :meta default-meta :validator default-validator))
  ([state & args]
    (let [config    (apply array-map args)
          meta      (get config :meta default-meta)
          validator (get config :validator default-validator)]
      (Atom. (ent/make-atomic-entity state)
             (ent/make-atomic-entity meta)
             (ent/make-atomic-entity validator)
             (ent/make-atomic-entity {})))))
