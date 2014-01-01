(ns clojure.lang.iassociative
  (:refer-clojure :only [defprotocol]))

(defprotocol IAssociative
  (-contains-key? [this k])
  (-entry-at      [this k])
  (-assoc         [this k v]))
