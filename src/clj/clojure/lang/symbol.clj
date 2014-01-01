(ns clojure.lang.symbol
  (:refer-clojure :only [defmacro deftype satisfies? defn defn- str let list* list and nil? declare cond last butlast first count concat zero?])
  (:require [clojure.lang.ordered              :refer [Ordered compare-to]]
            [clojure.lang.compare              :refer [compare]]
            [clojure.lang.equals               :refer [= not not=]]
            [clojure.lang.equivalence          :refer [Equivalence equivalent?]]
            [clojure.lang.hash                 :refer [Hash hash]]
            [clojure.lang.meta                 :refer [Meta]]
            [clojure.lang.named                :refer [Named name namespace]]
            [clojure.lang.platform.hash        :refer [platform-hash-method]]
            [clojure.lang.platform.equivalence :refer [platform-equals-method]]
            [clojure.lang.platform.ordered     :refer [platform-compare-to-method]]
            [clojure.lang.platform.object      :refer [instance? hash-combine]]
            [clojure.string]))

(defn- platform-symbol-methods []
  (concat
    (platform-compare-to-method)
    (platform-hash-method)
    (platform-equals-method)))

(defn- named-equivalence? [x-name y-name x-ns y-ns]
  (and (= x-name y-name)
       (= x-ns y-ns)))

(defn- equiv? [x y]
  (if (satisfies? Named y)
    (named-equivalence? (name x)
                        (name y)
                        (namespace x)
                        (namespace y))
    false))

(defn- named-compare [x y]
  (let [x-name (name x)
        y-name (name y)
        x-ns   (namespace x)
        y-ns   (namespace y)]
    (cond
      (named-equivalence? x-name y-name x-ns y-ns)
      0
      (and (nil? x-ns)
           (not (nil? y-ns)))
      -1
      (not (nil? x-ns))
      (cond
        (nil? y-ns) 1
        :else
        (let [num (compare x-ns y-ns)]
          (if (zero? num)
            (compare x-name y-name)
            num))))))

(defmacro defsymbol [type]
  (list*
    'deftype type ['ns 'name '-str '-hash 'meta]

    'Equivalence
    (list 'equivalent? ['this 'other]
          (list 'equiv? 'this 'other))

    'Hash
    (list 'hash ['this] '-hash)

    'Meta
    (list 'meta ['this] 'meta)
    (list 'with-meta ['this 'new-meta]
          (list 'new type 'ns 'name '-str '-hash 'new-meta))

    'Named
    (list 'name ['this] 'name)
    (list 'namespace ['this] 'ns)

    'Ordered
    (list 'compare-to ['this 'other] (list 'named-compare 'this 'other))

    (platform-symbol-methods)))

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
         (symbol (clojure.string/join "/" (butlast parts)) (last parts))))))
  ([ns name]
   (if (nil? name)
     (throw (Exception. "Can't create symbol with nil name")))
   (let [str (if ns
               (str ns "/" name)
               name)
         hash (hash-combine (hash name) (hash ns))]
     (Symbol. ns name str hash {}))))
