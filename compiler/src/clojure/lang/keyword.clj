(ns clojure.lang.keyword
  (:refer-clojure :only [defmacro deftype defn + let list* list str compare zero?])
  (:require [clojure.lang.hash     :refer [Hash hash]]
            [clojure.lang.meta     :refer [Meta]]
            [clojure.lang.named    :refer [Named name namespace]]
            [clojure.lang.operators :refer [==]]
            [clojure.lang.platform :refer [platform-keyword-methods instance?]]
            [clojure.lang.symbol   :refer [symbol]]))

(defmacro defkeyword [type]
  (list* 'deftype type ['ns 'name 'str 'hash 'meta 'sym]
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
