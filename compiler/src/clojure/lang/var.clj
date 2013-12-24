(ns clojure.lang.var
  (:require [clojure.lang.meta       :refer [Meta -meta]]
            [clojure.lang.namespaced :refer [Namespaced -namespace]]))

(deftype Unbound [var]
  Meta
  (-meta [this] {}))

(defn make-unbound [var]
  (Unbound. var))

(deftype Var [meta ns sym root]
  Meta
  (-meta [this] meta)

  Namespaced
  (-namespace [this] ns))

(defn make-var [meta ns sym root]
  (Var. meta ns sym root))

(defn -alter-var-root [var root]
  (reset! (.root var) root))
