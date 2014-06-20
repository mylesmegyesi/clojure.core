(ns clojure.lang.platform.random
  (:refer-clojure :only [defn])
  (:import [Math]))

(defn rand-float []
  (. Math (random)))
