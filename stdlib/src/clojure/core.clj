(create-ns clojure.core)
(in-ns     clojure.core)

(deftype* Symbol [ns name str])
(deftype* Var [sym root])

(def Symbol (rb-class* Symbol))
(def Var    (rb-class* Var))
