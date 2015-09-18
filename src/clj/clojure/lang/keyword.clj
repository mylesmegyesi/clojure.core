(ns clojure.lang.keyword
  (:refer-clojure :only [deftype defmacro declare defn let list list* concat -> +])
  (:require [clojure.lang.deftype     :refer [expand-methods]]
            [clojure.lang.comparison  :refer [platform-compare-to-method]]
            [clojure.lang.equivalence :refer [platform-equals-method]]
            [clojure.lang.hash        :refer [platform-hash-method]]
            [clojure.lang.show        :refer [platform-show-method]]
            [clojure.lang.protocols   :refer [INamed]]
            [clojure.next             :refer :all :exclude [+]]))

(declare is-keyword?)

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

(defmacro keyword-equals? [my-sym y]
  `(let [y# ~y]
     (if (is-keyword? y#)
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

    'INamed
    (list '-name ['this] '-name)
    (list '-namespace ['this] '-ns)

    platform-keyword-methods))

(defkeyword Keyword)

(defn is-keyword? [x]
  (instance? Keyword x))

(defn new-keyword [ns name str hash-code meta sym]
  (Keyword. ns name str hash-code meta sym))
