(ns clojure.ruby.analyzer
  (:refer-clojure :exclude [macroexpand-1 var?])
  (:require [clojure.tools.analyzer       :as ana :refer [analyze-fn-method empty-env analyze-in-env -analyze]]
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

(defmethod parse 'do [form env]
  (ana/-parse form env))

(defmethod parse 'fn* [form env]
  (ana/-parse form env))

(defmethod parse 'create-type* [[_ name fields :as form] env]
  (let [fields-expr (mapv (fn [name]
                            {:env     env
                             :form    name
                             :name    name
                             :local   :field
                             :op      :binding})
                          fields)]
    {:op         :create-type
     :env        env
     :form       form
     :name       name
     :fields     fields-expr
     :children   [:fields]}))

(defmethod parse :default [[f & args :as form] env]
  (if-not f
    (-analyze :const form env)
    (let [e (ctx env :expr)
          fn-expr (analyze f e)
          args-expr (mapv (analyze-in-env e) args)
          m (meta form)]
      (merge {:op   :invoke
              :form form
              :env  env
              :fn   fn-expr
              :args args-expr}
             (when m
               {:meta m}) ;; this implies it's not going to be evaluated
             {:children [:args :fn]}))))

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

(defn -let*
  [form env bindings & body]
  {:pre [(vector? bindings)
         (even? (count bindings))]}
  (loop [[binding & more] (reverse (seq (partition 2 bindings)))
         body body]
    (if-let [[name init] binding]
      (recur more (list (list* 'fn* [name] body) init))
      body)))

(def analyzer-macros {'let* -let*})

(defn macroexpand-1 [form env]
  (if (seq? form)
    (if-let [m (get analyzer-macros (first form))]
      (apply m form env (rest form))
      form)
    form))

  ;(if (seq? form)
  ;  (let [op (first form)
  ;        v (resolve-var op env)
  ;        m (meta v)
  ;        local? (-> env :locals (get op))
  ;        macro? (and (not local?) (:macro m))
  ;        ]
  ;    (cond

  ;      macro?
  ;      (apply v form env (rest form))

  ;      :else
  ;      (desugar-host-expr form env)))
  ;  (desugar-host-expr form env)))

(defn- wrap-current-ns [parser]
  (fn [form env]
    (parser form (assoc env :ns @current-ns))))

(defn analyze [form env]
  (binding [ana/macroexpand-1 macroexpand-1
            ana/create-var    (fn [sym _] sym)
            ana/parse         (wrap-current-ns parse)
            ana/var?          runtime-var?]
    (ana/analyze form env)))
