(ns clojure.lang.let
  (:refer-clojure :only [defmacro loop reverse fn seq partition if-let list list*]))

(defmacro let [bindings & body]
  (loop [[binding & more] (reverse (seq (partition 2 bindings)))
         body body]
    (if-let [[name init] binding]
      (recur more (list (list* 'clojure.core/fn [name] body) init))
      body)))
