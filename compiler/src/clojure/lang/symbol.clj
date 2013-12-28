(ns clojure.lang.symbol
  (:refer-clojure :only [deftype defn defn- str let require defmacro list* list and = nil? not declare cond compare not= delay assoc if-let last butlast first count]
                  :rename {compare core-compare})
  (:require [clojure.lang.hash     :refer [Hash]]
            [clojure.lang.meta     :refer [Meta]]
            [clojure.lang.named    :refer [Named name namespace]]
            [clojure.lang.platform :refer [instance? platform-symbol-methods symbol-hash-code]]
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

(defn- delay-equals [x y x-name y-name x-ns y-ns]
  (and (symbol? x)
       (symbol? y)
       (= @x-name @y-name)
       (= @x-ns @y-ns)))

(defn equals [x y]
  (delay-equals x y
                (delay (name x))
                (delay (name y))
                (delay (namespace x))
                (delay (namespace y))))

(defn- compare [x y]
  (let [x-name (delay (name x))
        y-name (delay (name y))
        x-ns   (delay (namespace x))
        y-ns   (delay (namespace y))]
    (cond
      (delay-equals x y x-name y-name x-ns y-ns)
      0
      (and (nil? @x-ns)
           (not (nil? @y-ns)))
      -1
      (not (nil? @x-ns))
      (cond
        (nil? @y-ns) 1
        :else
        (let [num (core-compare @x-ns
                                @y-ns)]
          (if (not= 0 num)
            num
            (core-compare @x-name
                          @y-name)))))))

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
