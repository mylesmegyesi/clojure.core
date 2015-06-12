(ns clojure.lang.atom
  (:refer-clojure :only [apply assoc cons defn defn- deftype dissoc empty? first fn let loop if-not into rest second])
  (:require [clojure.lang.atomic-ref :refer [new-atomic-ref ref-set!
                                             ref-get ref-compare-and-set!]]
            [clojure.lang.exceptions :refer [new-illegal-state-error]]
            [clojure.lang.protocols  :refer [IAtom IDeref IMeta IReference IValidatable IWatchable
                                             -reset-meta!]]
            [clojure.next            :refer :all :exclude [cons first second rest empty? assoc dissoc]]))

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

(deftype Atom [-state
               ^:unsynchronized-mutable -meta
               ^:volatile-mutable -validator
               ^:volatile-mutable -watches]
  IDeref
  (-deref [this] (ref-get -state))

  IMeta
  (-meta [this] -meta)

  IReference
  (-reset-meta! [this new-meta]
    (set! -meta new-meta)
    new-meta)

  (-alter-meta! [this f args]
    (let [meta-args (cons -meta args)
          new-meta (apply f meta-args)]
      (-reset-meta! this new-meta)))

  IAtom
  (-compare-and-set! [this old-state new-state]
    (do
      (validate-with-exception -validator new-state)
      (if (ref-compare-and-set! -state old-state new-state)
        (do
          (notify-watches -watches this old-state new-state)
          true)
        false)))

  (-reset! [this new-state]
    (let [old-state (ref-get -state)]
      (validate-with-exception -validator new-state)
      (ref-set! -state new-state)
      (notify-watches -watches this old-state new-state)
      new-state))

  (-swap! [this f args]
    (loop []
      (let [old-value (ref-get -state)
            new-value (apply f old-value args)]
        (validate-with-exception -validator new-value)
        (if (ref-compare-and-set! -state old-value new-value)
          (do
            (notify-watches -watches this old-value new-value)
            new-value)
          (recur)))))

  IValidatable
  (-get-validator [this] -validator)

  (-set-validator! [this f]
    (validate-with-exception f (ref-get -state))
    (set! -validator f) nil)

  IWatchable
  (-add-watch [this k f]
    (do
      (set! -watches (assoc -watches k f))
      this))

  (-remove-watch [this k]
    (set! -watches (dissoc -watches k))
    this))

(defn new-atom [state meta validator watches]
  (Atom. state meta validator watches))
