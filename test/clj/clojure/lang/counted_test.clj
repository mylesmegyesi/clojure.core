(ns clojure.lang.counted-test
  (:refer-clojure :only [])
  (:require [clojure.test           :refer :all]
            [clojure.lang.counted   :refer [count]]
            [clojure.lang.operators :refer [=]]))

(deftest nil-test
  (testing "(count nil) is 0"
    (is (= 0 (count nil)))))
