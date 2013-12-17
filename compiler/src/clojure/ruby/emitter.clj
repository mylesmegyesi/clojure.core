(ns clojure.ruby.emitter
  (:require [clojure.string :refer [join]]))

(def ^:dynamic *indent-level* 0)

(defmulti -emit (fn [{:keys [op]}] op))

(defmethod -emit :do [{:keys [statements ret]}]
  (let [emitted-statements (vec (map -emit statements))]
    (join "\n" (conj emitted-statements
                     (-emit ret)))))

(defmethod -emit :if [{:keys [test then else]}]
  (format "if %s\n%s\nelse\n%s\nend"
          (-emit test)
          (-emit then)
          (-emit else)))

(defmulti -invoke (fn [{:keys [fn]}] (:form fn)))

(defmethod -invoke '+ [{:keys [args]}]
  (if (= (count args) 1)
    (str (-emit (first args)))
    (reduce
      (fn [expr arg]
        (format "(%s + %s)" expr (-emit arg)))
      (-emit (first args))
      (rest args))))

(defmethod -invoke '=* [{:keys [args]}]
  (cond
    (= (count args) 0)
    (str "true")
    (= (count args) 1)
    (str "true")
    (= (count args) 2)
    (format "(%s == %s)" (-emit (first args)) (-emit (second args)))
    :else
    (loop [[x y & more] args comparisons []]
      (if (empty? more)
        (format "(%s)"
                (clojure.string/join " && "
                                     (conj (vec comparisons)
                                           (format "(%s == %s)" (-emit x) (-emit y)))))
        (recur (cons y more)
               (conj (vec comparisons) (format "(%s == %s)" (-emit x) (-emit y))))))))

(defmethod -invoke 'not [{:keys [args]}]
  (format "!%s" (-emit (first args))))

(defmethod -invoke 'assert [{:keys [args]}]
  (format "(raise RuntimeError.new(\"%s\") unless %s)"
          (format "%s is not true" (:form (first args)))
          (-emit (first args))))

(defmethod -emit :invoke [ast]
  (-invoke ast))

(defmethod -emit :const [{:keys [form]}]
  (str form))

(defn emit [ast]
  (-emit ast))
