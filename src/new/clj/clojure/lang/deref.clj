(ns clojure.lang.deref
  (:refer-clojure :only [defn])
  (:require [clojure.lang.ideref :refer [-deref]]))

(defn deref [obj]
  (-deref obj))
