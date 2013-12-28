(ns clojure.lang.rt
  (:require [clojure.lang.namespace :as namespace]))

(def *namespaces* (atom {}))
(def *current-ns* (atom 'user))

(defmacro -def [sym]
  `(namespace/-def *namespaces* ~sym))
