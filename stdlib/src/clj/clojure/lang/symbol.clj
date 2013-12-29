(ns clojure.lang.symbol
  (:refer-clojure :only [defmacro deftype satisfies? defn defn- str let list* list and nil? declare cond compare last butlast first count]
                  :rename {compare core-compare})
  (:require [clojure.lang.hash            :refer [Hash]]
            [clojure.lang.meta            :refer [Meta]]
            [clojure.lang.named           :refer [Named name namespace]]
            [clojure.lang.operators       :refer [= not not=]]
            [clojure.lang.platform.object :refer [instance?]]
            [clojure.lang.platform.symbol :refer [platform-symbol-methods symbol-hash-code]]
            [clojure.string]))

(declare equals compare make-symbol)

(defmacro defsymbol [type]
  (list*
    'deftype type ['ns 'name 'str 'hash 'meta]
    'Named
    (list 'name ['this] 'name)
    (list 'namespace ['this] 'ns)
    'Meta
    (list 'meta ['this] 'meta)
    (list 'with-meta ['this 'new-meta]
          (list 'make-symbol 'ns 'name 'str 'hash 'new-meta))
    'Hash
    (list 'hash ['this] 'hash)
    (platform-symbol-methods)))

(defsymbol Symbol)

(defn- make-symbol [ns name str hash meta]
  (Symbol. ns name str hash meta))

(defn symbol? [obj]
  (instance? Symbol obj))

(defn- loosely-equal? [x-name y-name x-ns y-ns]
  (and (= x-name y-name)
       (= x-ns y-ns)))

(defn equals [x y]
  (if (satisfies? Named y)
    (loosely-equal? (name x)
                    (name y)
                    (namespace x)
                    (namespace y))
    false))

(defn- compare [x y]
  (let [x-name (name x)
        y-name (name y)
        x-ns   (namespace x)
        y-ns   (namespace y)]
    (cond
      (loosely-equal? x-name y-name x-ns y-ns)
      0
      (and (nil? x-ns)
           (not (nil? y-ns)))
      -1
      (not (nil? x-ns))
      (cond
        (nil? y-ns) 1
        :else
        (let [num (core-compare x-ns
                                y-ns)]
          (if (not= 0 num)
            num
            (core-compare x-name
                          y-name)))))))

(defn symbol
  ([name]
   (if (symbol? name)
     name
     (let [parts (clojure.string/split name #"/")]
       (if (= 1 (count parts))
         (symbol nil (first parts))
         (symbol (clojure.string/join "/" (butlast parts)) (last parts))))))
  ([ns name]
   (let [str (if ns
               (str ns "/" name)
               name)]
     (if (nil? name)
       (throw (Exception. "Can't create symbol with nil name")))
     (make-symbol ns name str (symbol-hash-code ns name) {}))))
