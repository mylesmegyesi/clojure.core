(in-ns clojure.core) ; make me (in-ns 'clojure.core)

(deftype* Symbol [ns name str])
(deftype* Var [sym root])

(def Symbol Symbol)
(def Var    Var)

(def = (fn* [x y] (=* x y)))

(def not (fn* [x] (if x false true)))

(def assert (fn* [value] (if (not value) (throw (Exception. "false")))))
