(ns clojure.lang.sequential
  (:refer-clojure :only [defn satisfies?])
  (:require [clojure.lang.isequential :refer [ISequential]]))

(defn sequential? [s]
  (satisfies? ISequential s))
