(ns clojure.lang.seqable
  (:refer-clojure :only [defn extend-type fn])
  (:require [clojure.lang.iseqable :refer [ISeqable -seq]]
            [clojure.lang.platform.seqable]))

(defn seq [i]
  (-seq i))

(extend-type nil
  ISeqable
  (-seq [this] nil))
