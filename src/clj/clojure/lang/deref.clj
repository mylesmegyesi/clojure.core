(ns clojure.lang.deref
  (:refer-clojure :only [defn])
  (:require [clojure.lang.protocols :refer [-deref]]))

(defn deref [obj]
  (-deref obj))
