(ns clojure.lang.platform.arithmetic
  (:refer-clojure :only [extend-type fn ->])
  (:require [clojure.lang.iarithmetic      :refer [IArithmetic]]
            [clojure.lang.platform.numbers :refer [overflow-ops ops-add]]
            [clojure.lang.platform.object  :refer [instance? type]])
  (:import [java.lang Number]))

(extend-type Number
  IArithmetic
  (-add [x y]
    (if (instance? Number y)
      (-> (overflow-ops (type x) (type y))
        (ops-add x y))
      false)))
