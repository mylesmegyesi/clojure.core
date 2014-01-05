(ns clojure.lang.ihash
  (:refer-clojure :only [defprotocol extend-type fn]))

(defprotocol IHash
  "This protocol should not be directly used in a deftype. It should only
  be used to override an existing platform hash method."
  (-hash [this]))

(extend-type nil
  IHash
  (-hash [this] 0))
