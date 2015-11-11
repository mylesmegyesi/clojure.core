(ns clojure.lang.deftype
  (:refer-clojure :only [cond class? defmacro defn- macroexpand-1 fn last let list* list? map resolve str symbol symbol? ->])
  (:require [clojure.string          :refer [split]]
            [clojure.lang.exceptions :refer [platform-try exception new-exception]]))

(defn- symbol->Class [resolved-symbol original-symbol]
  (if (class? resolved-symbol)
    (-> (str resolved-symbol)
        (split #"\.")
        last
        symbol)
    original-symbol))

(defn- do-resolve [sym]
  (platform-try
    (let [r (resolve sym)]
      (if (class? r)
        r
        @r))
    (platform-catch exception e
      (throw (new-exception (str sym " could not be resolved"))))))

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

