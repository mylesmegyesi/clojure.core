(ns clojure.lang.deftype
  (:refer-clojure :only [cond class? defmacro defn- macroexpand-1 fn last let list* list? map resolve str symbol symbol? ->])
  (:require [clojure.string :refer [split]]))

(defn- symbol->Class [resolved-symbol original-symbol]
  (if (class? resolved-symbol)
    (-> (str resolved-symbol)
        (split #"\.")
        last
        symbol)
    original-symbol))

(defn- do-resolve [sym]
  (let [r (resolve sym)]
    (if (class? r)
      r
      @r)))

(defmacro deftype [t bs & body]
  (let [b (map #(cond
                  (symbol? %)
                    (symbol->Class (do-resolve %) %)
                  (list? %)
                    (macroexpand-1 %)
                  :else
                    %)
                body)]
    (list* 'clojure.core/deftype t bs b)))

