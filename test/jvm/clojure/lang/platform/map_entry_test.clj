(ns clojure.lang.platform.map-entry-test
  (:refer-clojure :only [let])
  (:require [clojure.test           :refer :all]
            [clojure.lang.map-entry :refer [new-map-entry]]))

(deftest map-entry-platform-test

  (testing "one map entry .equals another"
    (let [m1 (new-map-entry :k 1)
          m2 (new-map-entry :k 1)]
      (is (.equals m1 m2))))

  )
