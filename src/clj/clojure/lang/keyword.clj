(ns clojure.lang.keyword
  (:refer-clojure :only [deftype defmacro defn defn- + let list* list concat ->])
  (:require [clojure.lang.comparable           :refer [compare]]
            [clojure.lang.equivalence          :refer [=]]
            [clojure.lang.hash                 :refer [hash]]
            [clojure.lang.icomparable          :refer [IComparable]]
            [clojure.lang.iequivalence         :refer [IEquivalence]]
            [clojure.lang.ihash                :refer [IHash]]
            [clojure.lang.imeta                :refer [IMeta]]
            [clojure.lang.inamed               :refer [INamed]]
            [clojure.lang.ishow                :refer [IShow]]
            [clojure.lang.named                :refer [name namespace]]
            [clojure.lang.platform.equivalence :refer [platform-equals-method]]
            [clojure.lang.platform.hash        :refer [platform-hash-method]]
            [clojure.lang.platform.object      :refer [instance? hash-combine expand-methods]]
            [clojure.lang.platform.comparable  :refer [platform-compare-to-method]]
            [clojure.lang.platform.show        :refer [platform-show-method]]
            [clojure.lang.show                 :refer [str]]
            [clojure.lang.symbol               :refer [symbol]]))

(def platform-keyword-methods
  (-> {}
    platform-compare-to-method
    platform-hash-method
    platform-show-method
    platform-equals-method
    expand-methods))

(defmacro defkeyword [type]
  (list*
    'deftype type ['ns 'name '-str 'hash 'meta 'sym]

    'IComparable
    (list '-compare-to ['this 'other]
          (list 'compare 'sym 'other))

    'IEquivalence
    (list '-equivalent? ['this 'other]
          (list '= 'sym 'other))

    'IHash
    (list '-hash ['this] 'hash)

    'IMeta
    (list '-meta ['this] 'meta)
    (list '-with-meta ['this 'new-meta]
          (list 'new type 'ns 'name '-str 'hash 'new-meta 'sym))

    'INamed
    (list '-name ['this] 'name)
    (list '-namespace ['this] 'ns)

    'IShow
    (list '-show ['this] '-str)

    platform-keyword-methods))

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
