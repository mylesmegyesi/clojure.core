(ns clojure.ruby.runtime
  (:require [clojure.ruby.util :refer [namespace->module collase-modules]])
  (:import  [org.jruby.embed ScriptingContainer LocalContextScope]
            [org.jruby RubyObject RubyString]))

(defn new-runtime []
  (ScriptingContainer. LocalContextScope/SINGLETHREAD))

(defn eval-in-runtime [runtime string]
  (.runScriptlet runtime string))

(defn runtime-var? [obj]
  (when (instance? RubyObject obj)
    (let [classname (-> obj
                      (.callMethod "class")
                      (.callMethod "name"))]
      (when (instance? RubyString classname)
        (= "Clojure::Core::Var"
           (.asJavaString classname))))))

(defn- const-get [runtime obj const]
  (eval-in-runtime runtime (format "%s.const_get(\"%s\")" obj const)))

(defn lookup-var-in-module [runtime module-name ns sym]
  (when-let [const (first
                     (filter
                       (fn [constant-name]
                         (let [constant (const-get runtime module-name constant-name)]
                           (when (runtime-var? constant)
                             (let [found-sym (.callMethod constant "sym")]
                               (and
                                 (= (name ns)
                                    (.asJavaString (.callMethod found-sym "ns")))
                                 (= (name sym)
                                    (.asJavaString (.callMethod found-sym "name"))))))))
                       (eval-in-runtime runtime (format "%s.constants" module-name))))]
    (.asJavaString (.callMethod const "to_s"))))

(defn var->constant [runtime var]
  (let [sym (.callMethod var "sym")
        sym-name (.asJavaString (.callMethod sym "name"))
        var-ns (.asJavaString (.callMethod sym "ns"))
        module-name (namespace->module var-ns)]
    (when-let [const-name (lookup-var-in-module runtime module-name var-ns sym-name)]
      (collase-modules [module-name const-name]))))
