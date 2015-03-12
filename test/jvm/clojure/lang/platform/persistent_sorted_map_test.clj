(ns clojure.lang.platform.persistent-sorted-map-test
  (:refer-clojure :only [])
  (:require [clojure.test                              :refer :all]
            [clojure.lang.platform.persistent-map-test :refer [platform-map-test]]
            [clojure.next                              :refer [sorted-map]]))

(deftest sorted-map-platform-test
  (platform-map-test sorted-map))
