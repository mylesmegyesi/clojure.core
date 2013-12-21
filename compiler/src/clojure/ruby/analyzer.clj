(ns clojure.ruby.analyzer
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.tools.analyzer       :as ana :refer [analyze-fn-method empty-env]]
            [clojure.tools.analyzer.utils :refer [resolve-var]]
            [clojure.ruby.emitter         :refer [emit]])
  (:import [org.jruby.embed ScriptingContainer LocalContextScope]))

(declare analyze)

(defn analyzer-runtime []
  (ScriptingContainer. LocalContextScope/SINGLETHREAD))

(def current-ns (atom 'user))

(defn set-ns! [sym]
  (swap! current-ns (fn [old] sym)))

(defn initial-env []
  (assoc (empty-env) :runtime (analyzer-runtime)))

(defn eval-ast [ast]
  (let [code (emit ast)]
    (.runScriptlet (-> ast :env :runtime) code)))

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

(defmethod parse 'create-ns [[_ name :as form] env]
  (let [ast {:op   :create-ns
             :env  env
             :name name
             :form form}]
    (eval-ast ast)
    ast))

(defmethod parse 'in-ns [[_ name :as form] env]
  (set-ns! name)
  {:op   :in-ns
   :env  env
   :name name
   :form form})

(defmethod parse 'rb-class* [[_ name :as form] env]
  {:op   :rb-class*
   :env  env
   :name name
   :form form})

(defmethod parse 'def [form env]
  (let [ast (ana/-parse form env)]
    (eval-ast ast)
    ast))

(defmethod parse :default [form env]
  (ana/-parse form env))

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
        form))
    form))

(defn create-var
  [sym _]
  sym)

(defn- wrap-current-ns [parser]
  (fn [form env]
    (parser form (assoc env :ns @current-ns))))

(defn analyze [form env]
  (binding [ana/macroexpand-1 macroexpand-1
            ana/create-var    create-var
            ana/parse         (wrap-current-ns parse)]
    (ana/analyze form env)))
