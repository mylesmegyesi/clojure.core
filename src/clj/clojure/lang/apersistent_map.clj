(ns clojure.lang.apersistent-map
  (:refer-clojure :only [bit-and defmacro defn let loop + list list* concat ->])
  (:require [clojure.lang.deftype              :refer [expand-methods]]
            [clojure.lang.hash                 :refer [hash]]
            [clojure.lang.map-entry            :refer [key val]]
            [clojure.lang.platform.enumerable  :refer [platform-enumerable-method]]
            [clojure.lang.platform.equivalence :refer [platform-equals-method]]
            [clojure.lang.platform.hash        :refer [platform-hash-method]]
            [clojure.next                      :refer :all :exclude [+ bit-and]]))

(defn map-hash [m]
  (loop [entries (seq m)
         acc     0]
    (if entries
      (let [entry (first entries)]
        (recur
          (next entries)
          (+ acc (bit-and (hash (key entry))
                          (hash (val entry))))))
      acc)))

(defn map-equals? [m1 m2]
  (if (= (count m1) (count m2))
    (loop [m1-seq (seq m1)]
      (if m1-seq
        (let [first-entry (first m1-seq)
              k (key first-entry)
              v (val first-entry)]
          (if (and (contains? m2 k)
                   (= v (get m2 k)))
            (recur (next m1-seq))
            false))
        true))
    false))

(defmacro defmap [type bindings & body]
  (concat
    (list* 'clojure.core/deftype type bindings body)
    (-> {}
      (platform-hash-method 'clojure.lang.apersistent-map/map-hash)
      ;platform-show-method
      platform-enumerable-method
      (platform-equals-method 'clojure.lang.apersistent-map/map-equals?)
      expand-methods)))
