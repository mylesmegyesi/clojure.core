(ns clojure.lang.var
  (:refer-clojure :only [deftype defn str reset!])
  (:require [clojure.lang.meta  :refer [Meta]]
            [clojure.lang.named :refer [Named]]))

(deftype Unbound [var]
  Meta
  (meta [this] {}))

(defn make-unbound [var]
  (Unbound. var))

(deftype Var [meta ns sym root]
  Meta
  (meta [this] meta)

  Named
  (namespace [this] (str ns))
  (name      [this] (str sym)))

(defn make-var [meta ns sym root]
  (Var. meta ns sym root))

(defn -alter-var-root [var root]
  (reset! (.root var) root))
