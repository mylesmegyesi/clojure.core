(ns clojure.lang.symbol
  (:refer-clojure :only [declare defn defn- let cond])
  (:require [clojure.lang
              [comparison  :as    comparison]
              [deftype     :refer [deftype]]
              [equivalence :as    equiv]
              [hash        :as    hash-code]
              [object      :as    obj]
              [protocols   :refer [IMeta IObj INamed]]
              [show        :as    show]]
            [clojure.next :refer :all]))

(declare is-symbol? new-symbol)

(defn- named-equivalent? [s-name s-ns other]
  (and (= s-name (name other))
       (= s-ns (namespace other))))

(deftype Symbol [-ns -name -str -hash-code -meta]
  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (new-symbol -ns -name -str -hash-code m))

  INamed
  (-name [this] -name)

  (-namespace [this] -ns)

  obj/base-object
  (equiv/equals-method [this other]
    (if (is-symbol? other)
      (named-equivalent? -name -ns other)
      false))

  (hash-code/hash-method [this]
    -hash-code)

  (show/show-method [this]
    -str)

  comparison/base-comparable
  (comparison/comparison-method [this other]
    (cond
      (named-equivalent? -name -ns other)
        0
      (and (nil? -ns) (not (nil? (namespace other))))
        -1
      (not (nil? -ns))
        (let [other-name      (name other)
              other-namespace (namespace other)]
          (if (nil? other-namespace)
            1
            (let [n (compare -ns other-namespace)]
              (if (zero? n)
                (compare -name other-name)
                n)))))))

(defn is-symbol? [obj]
  (instance? Symbol obj))

(defn new-symbol [ns name str hash-code meta]
  (Symbol. ns name str hash-code meta))

