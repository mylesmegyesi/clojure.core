(ns clojure.lang.sequential
  (:refer-clojure :only [defn satisfies?])
  (:require [clojure.lang.protocols :refer [ISequential]]))

(defn sequential? [s]
  (satisfies? ISequential s))
