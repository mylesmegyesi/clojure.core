(ns clojure.lang.var
  (:refer-clojure :only [deftype defn reset!])
  (:require [clojure.lang.imeta           :refer [IMeta]]
            [clojure.lang.inamed          :refer [INamed]]
            [clojure.lang.operators       :refer [not]]
            [clojure.lang.platform.object :refer [instance?]]
            [clojure.lang.show            :refer [str]]
            ))

(deftype Unbound [var]
  IMeta
  (-meta [this] {}))

(defn make-unbound [var]
  (Unbound. var))

(deftype Var [meta ns sym root]
  IMeta
  (-meta [this] meta)

  INamed
  (-namespace [this] (str ns))
  (-name      [this] (str sym)))

(defn make-var [meta ns sym root]
  (Var. meta ns sym root))

(defn -alter-var-root [var root]
  (reset! (.root var) root))

(defn var? [thing]
  (instance? Var thing))

(defn bound? [thing]
  (not (instance? Unbound thing)))
