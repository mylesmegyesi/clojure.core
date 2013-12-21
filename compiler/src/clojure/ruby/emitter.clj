(ns clojure.ruby.emitter
  (:require [clojure.string]))

(defn- comma-separate [exprs]
  (clojure.string/join ", " exprs))

(defn- string-quote [s]
  (format "\"%s\"" s))

(defn- endl-separate [exprs]
  (clojure.string/join "\n" exprs))

(defn collase-modules [modules]
  (clojure.string/join "::" modules))

(defn ns->modules [ns-name]
  (map
    clojure.string/capitalize
    (clojure.string/split ns-name #"\.")))

(def namespace->module
  (memoize
    (fn [ns-name]
      (-> ns-name
        ns->modules
        collase-modules))))

(defn- module-eval [ns body]
  (if (and ns (not= 'user ns) (not= "" body))
    (format "%s.module_eval do\n%s\nend"
            (namespace->module (name ns))
            body)
    body))

(declare emit)

(defmulti -emit (fn [{:keys [op] :as ast}] op))

(defn- emit-var [ns sym root]
  (let [module-name (namespace->module (name ns))
        const-name (str "VAR_" (clojure.string/upper-case (name sym)))
        symbol (format "Clojure::Core::Symbol.new(%s)"
                       (comma-separate
                         (map string-quote [(name ns)
                                            (.getName sym)
                                            (str (name ns) "/" (.toString sym))])))
        new-var (format "Clojure::Core::Var.new(%s)" (comma-separate [symbol root]))]
    (format "%s.const_set(\"%s\", %s)" module-name const-name new-var)))

(defn- emit-type [ns name fields interfaces methods]
  (let [arg-names (map :name fields)
        arglist (clojure.string/join ", " arg-names)
        ivars  (clojure.string/join "\n " (map #(format "@%s = %s" % %) arg-names))
        constructor (format "def initialize(%s)\n%s\nend" arglist ivars)
        module (namespace->module (clojure.core/name ns))]
    (format "%s.const_set(\"%s\", Class.new do\n%s\nend)" module name constructor)))

(defmethod -emit :deftype [{:keys [name fields methods interfaces env] :as ast}]
  (emit-type (:ns env) name fields interfaces methods))

(defmethod -emit :def [{:keys [name env init] :as ast}]
  (emit-var (:ns env) name (-emit init)))

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

(defmethod -emit :create-ns [{:keys [name]}]
  (emit-module-declaration (clojure.core/name name)))

(defmethod -emit :do [{:keys [statements ret env] :as ast}]
  (format "begin\n%s\nend"
          (endl-separate (map emit (conj (vec statements) ret)))))

(defmethod -emit :in-ns [ast] "")

(defmethod -emit :rb-class* [{:keys [name env]}]
  (format "%s.const_get(\"%s\")"
          (namespace->module (clojure.core/name (:ns env)))
          name))

(defn- wrap-emit-in-module-eval [emitter]
  (fn [{:keys [op env] :as ast}]
    (let [body (emitter ast)]
      (if (= op :create-ns)
        body
        (module-eval (:ns env) body)))))

(defn- wrap-emit-source-comment [emitter]
  (fn [{:keys [form] :as ast}]
    (format "# %s\n%s" form (emitter ast))))

(def emitter (-> -emit
               wrap-emit-in-module-eval
               wrap-emit-source-comment
               ))

(defn emit [ast]
  (emitter ast))
