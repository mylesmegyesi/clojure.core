(ns clojure.ruby.emitter
  (:require [clojure.string]
            [clojure.tools.analyzer.utils :refer [resolve-var]]
            [clojure.ruby.runtime         :refer [var->constant lookup-var-in-module]]
            [clojure.ruby.util            :refer :all]))

(defn- endl-separate [exprs]
  (clojure.string/join "\n" exprs))

(defn- indent [s]
  (endl-separate
    (map
      #(str "  " %)
      (clojure.string/split s #"\n"))))

(defn- comma-separate [exprs]
  (clojure.string/join ", " exprs))

(defn wrap-parens [expr]
  (format "(%s)" expr))

(defn equals [x y]
  (format "%s == %s" x y))

(defn- wrap-quotes [s]
  (format "\"%s\"" s))

(defmulti -emit (fn [{:keys [op] :as ast}] op))

(defn- emit-var [{:keys [ns runtime]} sym root]
  (let [module-name (namespace->module (name ns))
        const-name   (or (lookup-var-in-module runtime module-name ns sym)
                       (clojure.string/upper-case (name (gensym "var"))))
        symbol (format "Clojure::Core::Symbol.new(%s)"
                       (comma-separate
                         (map wrap-quotes [(name ns)
                                            (.getName sym)
                                            (str (name ns) "/" (.toString sym))])))
        new-var (format "Clojure::Core::Var.new(%s)" (comma-separate [symbol root]))]
    (format "%s.const_set(\"%s\", %s)" module-name const-name new-var)))

(defn- emit-type [ns name fields interfaces methods]
  (let [arg-names (map :name fields)
        arglist (clojure.string/join ", " arg-names)
        ivar   #(format "@%s = %s" % %)
        ivars (endl-separate (map ivar arg-names))
        attr-reader #(format "attr_reader :%s" %)
        attr-readers (endl-separate (map attr-reader arg-names))
        constructor (format "def initialize(%s)\n%s\nend" arglist (indent ivars))
        body (endl-separate [attr-readers constructor])]
    (format "%s.const_set(\"%s\", Class.new do\n%s\nend)"
            (namespace->module (clojure.core/name ns))
            name
            (indent body))))

(defmethod -emit :deftype [{:keys [name fields methods interfaces env] :as ast}]
  (emit-type (:ns env) name fields interfaces methods))

(defmethod -emit :def [{:keys [name env init] :as ast}]
  (emit-var env name (-emit init)))

(defmethod -emit :maybe-class [{:keys [class env]}]
  (format "%s.const_get(\"%s\")"
          (namespace->module (name (:ns env)))
          class))

(defmethod -emit :binding [{:keys [name arg-id] :as ast}]
  (format "%s = args[%s]" name arg-id))

(defmethod -emit :fn-method [{:keys [params body] :as ast}]
  (endl-separate (map -emit (conj (vec params) body))))

(defmethod -emit :fn [{:keys [methods env] :as ast}]
  (format "%s.const_set(\"%s\", Class.new do\n%s\nend.new)"
          (namespace->module (name (:ns env)))
          (clojure.string/upper-case (name (gensym "fn")))
          (indent
            (format "def invoke(*args)\n%s\nend"
                    (indent (-emit (first methods)))))))

(defmethod -emit :local [{:keys [name] :as ast}]
  (str name))

(defmethod -emit :const [{:keys [val type] :as ast}]
  (case type
    :string (wrap-quotes val)
    (str val)))

(defmethod -emit :var [{:keys [var env] :as ast}]
  (var->constant (:runtime env) var))

(defmethod -emit :invoke [{:keys [fn args] :as ast}]
  (format "%s.invoke(%s)"
          (-emit fn)
          (comma-separate (map -emit args))))

(defmethod -emit :new [{:keys [args env class] :as ast}]
  (format "%s.new(%s)"
          (-emit {:op :maybe-class
                  :env env
                  :form class
                  :class class})
          (comma-separate (map -emit args))))

(defmethod -emit :if [{:keys [test then else] :as ast}]
  (let [else-expr (-emit else)]
    (if (= "" else-expr)
      (format "if %s\n%s\nend"
              (-emit test)
              (indent (-emit then)))
      (format "if %s\n%s\nelse\n%s\nend"
              (-emit test)
              (indent (-emit then))
              (indent else-expr)))))

(defmethod -emit :do [{:keys [statements ret env] :as ast}]
  (endl-separate (map -emit (conj (vec statements) ret))))

(defmethod -emit :throw [{:keys [exception] :as ast}]
  (format "raise %s" (-emit exception)))

(defmethod -emit :=* [{:keys [args]}]
  (if (> (count args) 1)
    (loop [[x y & more] args exprs []]
      (let [expr (wrap-parens (equals (-emit x) (-emit y)))]
        (if (empty? more)
          (if (empty? exprs)
            expr
            (wrap-parens
              (clojure.string/join
                " && "
                (conj exprs expr))))
          (recur (cons y more) (vec (conj exprs expr))))))
    "true"))

(defn- fully-qualify-modules [modules]
  (loop [modules modules qualified []]
    (if (empty? modules)
      qualified
      (recur (butlast modules)
             (cons (collase-modules modules) qualified)))))

(defn- format-as-module-definition [module]
  (format "module %s; end" module))

(def emit-module-declaration
  (memoize
    (fn [ns-name]
      (-> ns-name
        ns->modules
        fully-qualify-modules
        (#(map format-as-module-definition %))
        endl-separate))))

(defmethod -emit :in-ns [{:keys [name]}]
  (emit-module-declaration (clojure.core/name name)))

(defn emit [ast]
  (-emit ast))
