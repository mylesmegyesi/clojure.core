(ns clojure.lang.iarithmetic
  (:refer-clojure :only [defprotocol]))

(defprotocol IArithmetic
  (-add [x y])
  (-subtract [x y])
  (-multiply [x y])
  (-divide   [x y])
  (-modulus  [x y])
  (-exponent [x y]))
