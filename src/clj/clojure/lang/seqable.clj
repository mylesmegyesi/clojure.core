(ns clojure.lang.seqable
  (:refer-clojure :only [defn extend-type fn])
  (:require [clojure.lang.platform.seqable]
            [clojure.lang.protocols :refer [ISeqable -seq]]))

(defn seq [i]
  (-seq i))

(extend-type nil
  ISeqable
  (-seq [this] nil))
