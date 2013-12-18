(ns clojure.ruby.emitter
  (:require [clojure.string :refer [join]]))

(defmulti -emit (fn [{:keys [op] :as ast}]
                  op))

(defmethod -emit nil [ast]
  (println "nil ast")
  (prn ast))

(defmethod -emit :do [{:keys [statements ret]}]
  (let [emitted-statements (vec (map -emit statements))]
    (join "\n" (conj emitted-statements
                     (-emit ret)))))

(defmethod -emit :if [{:keys [test then else]}]
  (format "if %s\n%s\nelse\n%s\nend"
          (-emit test)
          (-emit then)
          (-emit else)))

;(defmulti -invoke (fn [{:keys [fn]}] (:form fn)))
;
;(defmethod -invoke '+ [{:keys [args]}]
;  (if (= (count args) 1)
;    (str (-emit (first args)))
;    (reduce
;      (fn [expr arg]
;        (format "(%s + %s)" expr (-emit arg)))
;      (-emit (first args))
;      (rest args))))
;
;(defn- -emit= [x y]
;  (format "(%s == %s)" (-emit x) (-emit y)))

;(defmethod -invoke '=* [{:keys [args]}]
;  (format "Clojure::Lang.special(\"=*\").invoke(%s)"
;          (clojure.string/join ", " (map -emit args))))
;
;(defmethod -invoke 'not [{:keys [args]}]
;  (format "!%s" (-emit (first args))))
;
;(defmethod -invoke 'assert [{:keys [args]}]
;  (format "(raise RuntimeError.new(\"%s\") unless %s)"
;          (format "%s is not true" (:form (first args)))
;          (-emit (first args))))

(defn -emit-sym [name]
  (format "Clojure::Lang::Symbol.new(\"%s\")" name))

(def specials #{'ns '=*})

(defmethod -emit :maybe-class [{:keys [form] :as ast}]
  (cond
    (specials form)
    (format "Clojure::Lang.special(\"%s\")" form)
    (symbol? form)
    (-emit-sym form)
    :else
    (throw (Exception. "wat"))))

(defmethod -emit :def [{:keys [name init] :as ast}]
  (format "Clojure::RT.current_ns.intern(%s).set_root(%s)"
          (-emit-sym name)
          (-emit init)))

(defmethod -emit :invoke [{:keys [fn args]}]
  (format "%s.invoke(%s)"
          (-emit fn)
          (clojure.string/join ", " (map -emit args))))

(defmethod -emit :const [{:keys [form]}]
  (str form))

(defn emit [ast]
  (-emit ast))
