(ns clojure.lang.keyword
  (:refer-clojure :only [deftype defmacro declare defn defn- + let list* list concat ->])
  (:require [clojure.lang.comparison           :refer [compare]]
            [clojure.lang.deftype              :refer [expand-methods]]
            [clojure.lang.hash                 :refer [hash]]
            [clojure.lang.imeta                :refer [IMeta]]
            [clojure.lang.inamed               :refer [INamed]]
            [clojure.lang.named                :refer [name namespace]]
            [clojure.lang.object               :refer [instance?]]
            [clojure.lang.operators            :refer [=]]
            [clojure.lang.platform.comparison  :refer [platform-compare-to-method]]
            [clojure.lang.platform.equivalence :refer [platform-equals-method]]
            [clojure.lang.platform.hash        :refer [platform-hash-method]]
            [clojure.lang.platform.show        :refer [platform-show-method]]
            [clojure.lang.show                 :refer [str]]
            [clojure.lang.symbol               :refer [symbol]]))

(defmacro keyword-compare-init
  {:private true}
  [_ other-arg]
  (list 'compare '-sym other-arg))

(defmacro keyword-hash-init
  {:private true}
  [_] '-hash-code)

(defmacro keyword-str-init
  {:private true}
  [_] '-str)

(declare keyword?)

(defmacro keyword-equals? [my-sym y]
  `(let [y# ~y]
     (if (keyword? y#)
       (= ~my-sym (symbol (namespace y#)
                          (name y#)))
       false)))

(defmacro keyword-equals?-init
  {:private true}
  [_ other-arg]
  (list 'keyword-equals? '-sym other-arg))

(def platform-keyword-methods
  (-> {}
    (platform-compare-to-method 'keyword-compare-init)
    (platform-hash-method 'keyword-hash-init)
    (platform-show-method 'keyword-str-init)
    (platform-equals-method 'keyword-equals?-init)
    expand-methods))

(defmacro defkeyword [type]
  (list*
    'deftype type ['-ns '-name '-str '-hash-code '-meta '-sym]

    'IMeta
    (list '-meta ['this] '-meta)
    (list '-with-meta ['this 'new-meta]
          (list 'new type '-ns '-name '-str '-hash-code 'new-meta '-sym))

    'INamed
    (list '-name ['this] '-name)
    (list '-namespace ['this] '-ns)

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
         hash-code (hash (+ (hash sym) 0x9e3779b9))]
     (Keyword. ns name (str ":" sym) hash-code {} sym))))
