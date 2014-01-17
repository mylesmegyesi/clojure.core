(ns clojure.lang.operators
  (:refer-clojure :only [defmacro defn apply = == not not= and or]
                  :rename {= core-eq
                           == core-equiv
                           not core-not
                           not= core-not=
                           and core-and
                           or core-or}))

(def = core-eq)
(def == core-equiv)
(def not core-not)
(def not= core-not=)
(defn not== [& args] (not (apply == args)))

(defmacro and [& body]
  `(core-and ~@body))

(defmacro or [& body]
  `(core-or ~@body))
