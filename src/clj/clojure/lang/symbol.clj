(ns clojure.lang.symbol
  (:refer-clojure :only [defmacro deftype satisfies? defn defn- let list* list and nil? cond last butlast first count ->])
  (:require [clojure.lang.comparable           :refer [compare]]
            [clojure.lang.equivalence          :refer [= not]]
            [clojure.lang.hash                 :refer [hash]]
            [clojure.lang.icomparable          :refer [IComparable]]
            [clojure.lang.iequivalence         :refer [IEquivalence]]
            [clojure.lang.ihash                :refer [IHash]]
            [clojure.lang.imeta                :refer [IMeta]]
            [clojure.lang.inamed               :refer [INamed]]
            [clojure.lang.ishow                :refer [IShow]]
            [clojure.lang.named                :refer [name namespace]]
            [clojure.lang.platform.comparable  :refer [platform-compare-to-method]]
            [clojure.lang.platform.equivalence :refer [platform-equals-method]]
            [clojure.lang.platform.hash        :refer [platform-hash-method]]
            [clojure.lang.platform.object      :refer [instance? hash-combine expand-methods]]
            [clojure.lang.platform.show        :refer [platform-show-method]]
            [clojure.lang.show                 :refer [str]]
            [clojure.string]))

(def platform-symbol-methods
  (-> {}
    platform-compare-to-method
    platform-hash-method
    platform-show-method
    platform-equals-method
    expand-methods))

(defn- named-equivalence? [x-name y-name x-ns y-ns]
  (and (= x-name y-name)
       (= x-ns y-ns)))

(defn- equiv? [x y]
  (if (satisfies? INamed y)
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
          (if (= num 0)
            (compare x-name y-name)
            num))))))

(defmacro defsymbol [type]
  (list*
    'deftype type ['ns 'name '-str 'hash-code 'meta]

    'IComparable
    (list '-compare-to ['this 'other]
          (list 'named-compare 'this 'other))

    'IEquivalence
    (list '-equivalent? ['this 'other]
          (list 'equiv? 'this 'other))

    'IHash
    (list '-hash ['this] 'hash-code)

    'IMeta
    (list '-meta ['this] 'meta)
    (list '-with-meta ['this 'new-meta]
          (list 'new type 'ns 'name '-str 'hash-code 'new-meta))

    'INamed
    (list '-name ['this] 'name)
    (list '-namespace ['this] 'ns)

    'IShow
    (list '-show ['this] '-str)

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
         (symbol (clojure.string/join "/" (butlast parts)) (last parts))))))
  ([ns name]
   (if (nil? name)
     (throw (Exception. "Can't create symbol with nil name")))
   (let [str (if ns
               (str ns "/" name)
               name)
         hash (hash-combine (hash name) (hash ns))]
     (Symbol. ns name str hash {}))))
