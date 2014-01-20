(ns clojure.lang.platform.persistent-array-map-test
  (:refer-clojure :only [])
  (:require [clojure.test                              :refer :all]
            [clojure.lang.platform.persistent-map-test :refer [platform-map-test]]
            [clojure.lang.persistent-array-map         :refer [array-map]]))

(deftest array-map-platform-test
  (platform-map-test array-map))
