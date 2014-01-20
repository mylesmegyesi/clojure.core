(ns clojure.lang.persistent-array-map-test
  (:refer-clojure :only [])
  (:require [clojure.test                      :refer :all]
            [clojure.lang.persistent-map-test  :refer [map-test]]
            [clojure.lang.persistent-array-map :refer [array-map]]))

(deftest array-map-test
  (map-test "PersistentArrayMap" array-map))
