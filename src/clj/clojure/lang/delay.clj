(ns clojure.lang.delay
  (:refer-clojure :only [deftype defn when locking])
  (:require [clojure.next           :refer :all]
            [clojure.lang.protocols :refer [IDeref -deref IPending -is-realized?]]))

(deftype Delay [^:volatile-mutable -fn ^:volatile-mutable -value]
  IDeref
  (-deref [this]
    (locking this
      (when -fn
        (set! -value (-fn))
        (set! -fn nil))
      -value))

  IPending
  (-is-realized? [this]
    (locking this
      (nil? -fn))))

(defn is-delay? [d]
  (instance? Delay d))

(defn -force [obj]
  (if (instance? Delay obj)
    (-deref obj)
    obj))

(defn new-delay [fn]
  (Delay. fn nil))
