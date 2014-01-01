(ns clojure.lang.keyword
  (:refer-clojure :only [deftype defmacro defn defn- + let list* list str zero? concat])
  (:require [clojure.lang.compare              :refer [compare]]
            [clojure.lang.equals               :refer [=]]
            [clojure.lang.equivalence          :refer [Equivalence equivalent?]]
            [clojure.lang.hash                 :refer [Hash hash]]
            [clojure.lang.meta                 :refer [Meta]]
            [clojure.lang.named                :refer [Named name namespace]]
            [clojure.lang.ordered              :refer [Ordered compare-to]]
            [clojure.lang.platform.equivalence :refer [platform-equals-method]]
            [clojure.lang.platform.hash        :refer [platform-hash-method]]
            [clojure.lang.platform.object      :refer [instance? hash-combine]]
            [clojure.lang.platform.ordered     :refer [platform-compare-to-method]]
            [clojure.lang.symbol               :refer [symbol]]))

(defn- platform-keyword-methods []
  (concat
    (platform-compare-to-method)
    (platform-hash-method)
    (platform-equals-method)))

(defmacro defkeyword [type]
  (list*
    'deftype type ['ns 'name 'str 'hash 'meta 'sym]

    'Ordered
    (list 'compare-to ['this 'other] ('compare 'sym 'other))

    'Equivalence
    (list 'equivalent? ['this 'other] ('= 'sym 'other))

    'Hash
    (list 'hash ['this] 'hash)

    'Meta
    (list 'meta ['this] 'meta)
    (list 'with-meta ['this 'new-meta]
          (list 'new type 'ns 'name 'str 'hash 'new-meta 'sym))

    'Named
    (list 'name ['this] 'name)
    (list 'namespace ['this] 'ns)

    (platform-keyword-methods)))

(defkeyword Keyword)

(defn keyword? [obj]
  (instance? Keyword obj))

(defn keyword
  ([n]
   (let [sym (symbol n)]
     (keyword (namespace sym) (name sym))))
  ([ns name]
   (let [sym (symbol ns name)
         hash (+ (hash sym) 0x9e3779b9)]
     (Keyword. ns name (str ":" sym) hash {} sym))))
