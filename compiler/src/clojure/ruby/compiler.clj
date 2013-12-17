(ns clojure.ruby.compiler
  (:refer-clojure :exclude [read-string compile])
  (:require [clojure.tools.reader.edn :refer [read-string]]
            [clojure.ruby.analyzer    :refer [analyze]]
            [clojure.ruby.emitter     :refer [emit]]))

(defn compile [string env]
  (-> string
    read-string
    (analyze env)
    emit))
