(ns clojure.lang.keyword
  (:refer-clojure :only [declare defn defn- let])
  (:require [clojure.lang
              [deftype     :refer [deftype]]
              [comparison  :as    comparison]
              [equivalence :as    equiv]
              [hash        :as    hash-code]
              [object      :as    obj]
              [show        :as    show]
              [protocols   :refer [INamed]]]
            [clojure.next :refer :all]))

(declare is-keyword?)

(defn- keyword-equals? [my-sym y]
  (if (is-keyword? y)
    (= my-sym (symbol (namespace y)
                      (name y)))
    false))

(deftype Keyword [-ns -name -str -hash-code -meta -sym]
  INamed
  (-name [this] -name)

  (-namespace [this] -ns)

  obj/base-object
  (equiv/equals-method [this other]
    (keyword-equals? -sym other))

  (hash-code/hash-method [this]
    -hash-code)

  (show/show-method [this]
    -str)

  comparison/base-comparable
  (comparison/comparison-method [this other]
    (compare -sym other)))

(defn is-keyword? [x]
  (instance? Keyword x))

(defn new-keyword [ns name str hash-code meta sym]
  (Keyword. ns name str hash-code meta sym))

