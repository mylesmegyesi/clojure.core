(ns clojure.lang.symbol
  (:refer-clojure :only [defmacro deftype declare satisfies? defn defn- let list* list nil? cond rest first count ->])
  (:require [clojure.lang.comparison          :refer [compare]]
            [clojure.lang.hash                :refer [hash]]
            [clojure.lang.imeta               :refer [IMeta]]
            [clojure.lang.inamed              :refer [INamed]]
            [clojure.lang.named               :refer [name namespace]]
            [clojure.lang.hash                :refer [hash]]
            [clojure.lang.imeta               :refer [IMeta]]
            [clojure.lang.inamed              :refer [INamed]]
            [clojure.lang.named               :refer [name namespace]]
            [clojure.lang.operators           :refer [and not =]]
            [clojure.lang.platform.comparison :refer [platform-compare-to-method platform-equals-method]]
            [clojure.lang.platform.hash       :refer [platform-hash-method]]
            [clojure.lang.platform.object     :refer [instance? expand-methods hash-combine]]
            [clojure.lang.platform.show       :refer [platform-show-method]]
            [clojure.lang.show                :refer [str]]
            [clojure.string]))

(defmacro named-equivalence?
  {:private true}
  [x-name y-name x-ns y-ns]
  `(and (= ~x-name ~y-name)
        (= ~x-ns ~y-ns)))

(declare symbol?)

(defmacro symbol-equals?
  {:private true}
  [x-ns x-name y]
  `(let [y# ~y]
     (if (symbol? y#)
       (named-equivalence? ~x-name
                           (name y#)
                           ~x-ns
                           (namespace y#))
       false)))

(defmacro symbol-equals?-init
  {:private true}
  [_ other-arg]
  (list 'symbol-equals? '-ns '-name other-arg))

(defmacro symbol-compare
  {:private true}
  [x-ns x-name y]
  `(let [y# ~y
         x-name# ~x-name
         y-name# (name y#)
         x-ns#   ~x-ns
         y-ns#   (namespace y#)]
     (cond
       (named-equivalence? x-name# y-name# x-ns# y-ns#)
       0
       (and (nil? x-ns#)
            (not (nil? y-ns#)))
       -1
       (not (nil? x-ns#))
       (cond
         (nil? y-ns#) 1
         :else
         (let [num# (compare x-ns# y-ns#)]
           (if (= num# 0)
             (compare x-name# y-name#)
             num#))))))

(defmacro symbol-compare-init
  {:private true}
  [_ other-arg]
  (list 'symbol-compare '-ns '-name other-arg))

(defmacro symbol-hash-init
  {:private true}
  [_] '-hash-code)

(defmacro symbol-str-init
  {:private true}
  [_] '-str)

(def platform-symbol-methods
  (-> {}
    (platform-hash-method 'symbol-hash-init)
    (platform-show-method 'symbol-str-init)
    (platform-compare-to-method 'symbol-compare-init)
    (platform-equals-method 'symbol-equals?-init)
    expand-methods))

(defmacro defsymbol [type]
  (list*
    'deftype type ['-ns '-name '-str '-hash-code '-meta]

    'IMeta
    (list '-meta ['this] '-meta)
    (list '-with-meta ['this 'new-meta]
          (list 'new type '-ns '-name '-str '-hash-code 'new-meta))

    'INamed
    (list '-name ['this] '-name)
    (list '-namespace ['this] '-ns)

    platform-symbol-methods))

(defsymbol Symbol)

(defn symbol? [obj]
  (instance? Symbol obj))

(defn symbol
  ([name]
   (if (symbol? name)
     name
     (let [parts (clojure.string/split name #"/")]
       (if (= 1 (count parts))
         (symbol nil (first parts))
         (symbol (first parts) (clojure.string/join "/" (rest parts)))))))
  ([ns name]
   (if (nil? name)
     (throw (Exception. "Can't create symbol with nil name")))
   (Symbol. ns name (if ns (str ns "/" name) name)
            (hash (hash-combine (hash name) (hash ns))) nil)))
