(ns clojure.lang.platform.persistent-hash-map-test
  (:refer-clojure :only [])
  (:require [clojure.test                              :refer :all]
            [clojure.lang.platform.persistent-map-test :refer [platform-map-test]]
            [clojure.next                              :refer :all]))

(deftest hash-map-platform-test
  (platform-map-test hash-map))
