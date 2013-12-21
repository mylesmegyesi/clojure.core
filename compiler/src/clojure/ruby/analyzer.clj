(ns clojure.ruby.analyzer
  (:refer-clojure :exclude [macroexpand-1 var?])
  (:require [clojure.tools.analyzer       :as ana :refer [analyze-fn-method empty-env analyze-in-env]]
            [clojure.tools.analyzer.utils :refer [resolve-var ctx]]
            [clojure.ruby.emitter         :refer [emit]]
            [clojure.ruby.runtime         :refer :all]))

(declare analyze)

(def current-ns (atom 'user))

(defn set-ns! [sym]
  (swap! current-ns (fn [old] sym)))

(defn initial-env []
  (assoc (empty-env) :runtime (new-runtime)))

(defn eval-ast [{:keys [env] :as ast}]
  (eval-in-runtime (:runtime env) (emit ast)))

(defmulti parse (fn [[op & rest] env] op))

(defn analyze-method-impls
  [[name [this & params :as args] & body :as form] env]
  {:pre [(symbol? name)
         (vector? args)
         this]}
  (let [meth (cons params body)
        this-expr {:name  this
                   :env   env
                   :form  this
                   :op    :binding
                   :tag   (:this env)
                   :local :this}
        env (assoc-in (dissoc env :this) [:locals this] this-expr)
        method (analyze-fn-method meth env)]
    (assoc (dissoc method :variadic?)
           :op       :method
           :form     form
           :this     this-expr
           :name     (symbol (clojure.core/name name))
           :children (into [:this] (:children method)))))

(defn- maybe-class [thing]
  thing)

(defmethod parse 'deftype* [[_ name fields _ interfaces & methods :as form] env]
  (let [interfaces (disj (set (mapv maybe-class interfaces)) Object)
        fields-expr (mapv (fn [name]
                            {:env     env
                             :form    name
                             :name    name
                             :local   :field
                             :op      :binding})
                          fields)
        menv (assoc env
                    :context :expr
                    :locals (zipmap fields fields-expr)
                    :this name)
        methods* (mapv #(assoc (analyze-method-impls % menv) :interfaces interfaces) methods)
        ast {:op         :deftype
             :env        env
             :form       form
             :name       name
             :fields     fields-expr
             :methods    methods*
             :interfaces interfaces
             :children   [:fields :methods]}]

    (eval-ast ast)
    ast))

(defmethod parse 'in-ns [[_ name :as form] env]
  (set-ns! name)
  (let [ast {:op   :in-ns
             :env  env
             :name name
             :form form}]
    (eval-ast ast)
    ast))

(defmethod parse '=* [[_ & args :as form] env]
  {:op   :=*
   :env  env
   :args (mapv (analyze-in-env (ctx env :expr)) args)
   :form form})

(defmethod parse 'def [form env]
  (let [{:keys [env name] :as ast} (ana/-parse form env)
        {:keys [ns namespaces]} env
        var (eval-ast ast)]
    (swap! namespaces assoc-in [ns :mappings name] var)
    (assoc ast :var var)))

(defmethod parse :default [form env]
  (ana/-parse form env))

(defn desugar-host-expr [form env]
  (cond
    (seq? form)
    (let [[op & expr] form]
      (if (symbol? op)
        (let [opname (name op)]
          (cond
            (= (last opname) \.) ;; (class. ..)
            (list* 'new (symbol (subs opname 0 (dec (count opname)))) expr)
            :else form))))

    :else form))

(defn macroexpand-1 [form env]
  (if (seq? form)
    (let [op (first form)
          v (resolve-var op env)
          m (meta v)
          local? (-> env :locals (get op))
          macro? (and (not local?) (:macro m))
          ]
      (cond

        macro?
        (apply v form env (rest form))

        :else
        (desugar-host-expr form env)))
    (desugar-host-expr form env)))

(defn- wrap-current-ns [parser]
  (fn [form env]
    (parser form (assoc env :ns @current-ns))))

(defn analyze [form env]
  (binding [ana/macroexpand-1 macroexpand-1
            ana/create-var    (fn [sym _] sym)
            ana/parse         (wrap-current-ns parse)
            ana/var?          runtime-var?]
    (ana/analyze form env)))
