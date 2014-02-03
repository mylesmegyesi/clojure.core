(ns clojure.lang.var
  (:refer-clojure :only [deftype defn reset!])
  (:require [clojure.lang.protocols :refer [IMeta INamed]]
            [clojure.lang.show      :refer [str]]))

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
