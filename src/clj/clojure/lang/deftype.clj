(ns clojure.lang.deftype
  (:refer-clojure :only [cond class? defmacro defn defn- reduce macroexpand-1 concat cons fn last let list* list? map key val resolve str symbol symbol? ->])
  (:require [clojure.string :refer [split]]))

(defn expand-methods [methods]
  (reduce
    (fn [acc entry]
      (let [protocol (key entry)]
        (concat (cons protocol (val entry))
                acc)))
    []
    methods))

(defn- symbol->Class [resolved-symbol original-symbol]
  (if (class? resolved-symbol)
    (-> (str resolved-symbol)
        (split #"\.")
        last
        symbol)
    original-symbol))

(defmacro deftype [t bs & body]
  (let [b (map #(cond
                  (symbol? %)
                    (symbol->Class @(resolve %) %)
                  (list? %)
                    (macroexpand-1 %)
                  :else
                    %)
                body)]
    (list* 'clojure.core/deftype t bs b)))

