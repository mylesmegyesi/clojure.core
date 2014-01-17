(ns clojure.lang.iatom
  (:refer-clojure :refer [defprotocol]))

(defprotocol IAtom
  (-compare-and-set! [this old-state new-state])
  (-reset! [this new-state])
  (-swap! [this f args]))
