(ns clojure.lang.platform.atomic-entity
  (:refer-clojure :only [defn])
  (:import [java.util.concurrent.atomic AtomicReference]))

(defn get-entity [entity]
  (.get entity))

(defn set-entity! [entity new-value]
  (.set entity new-value))

(defn compare-and-set-entity! [entity old-value new-value]
  (.compareAndSet entity old-value new-value))

(defn make-atomic-entity [value]
  (AtomicReference. value))
