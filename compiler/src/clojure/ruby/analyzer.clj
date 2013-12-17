(ns clojure.ruby.analyzer
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.tools.analyzer                         :as ana]
            [clojure.tools.analyzer.utils                   :refer [ctx maybe-var]]
            [clojure.tools.analyzer.passes                  :refer [walk prewalk postwalk cycling]]
            [clojure.tools.analyzer.passes.source-info      :refer [source-info]]
            [clojure.tools.analyzer.passes.trim-do          :refer [trim-do]]
            [clojure.tools.analyzer.passes.cleanup          :refer [cleanup]]
            [clojure.tools.analyzer.passes.elide-meta       :refer [elide-meta]]
            [clojure.tools.analyzer.passes.constant-lifter  :refer [constant-lift]]
            [clojure.tools.analyzer.passes.warn-earmuff     :refer [warn-earmuff]]
            [clojure.tools.analyzer.passes.collect          :refer [collect]]
            [clojure.tools.analyzer.passes.add-binding-atom :refer [add-binding-atom]]
            [clojure.tools.analyzer.passes.uniquify         :refer [uniquify-locals]]))

(def specials ana/specials)

(defmulti parse (fn [[op & rest] env] op))

(defmethod parse :default
  [form env]
  (ana/-parse form env))

(defn desugar-host-expr [form env]
  form)

(defn macroexpand-1 [form env] form)

(defn create-var [sym {:keys [ns] :as what-is-this}]
  (keyword sym))

(defn run-passes
  "Applies the following passes in the correct order to the AST:
   * uniquify
   * add-binding-atom
   * cleanup
   * source-info
   * elide-meta
   * constant-lifter
   * warn-earmuff
   * collect"
  [ast]
  (-> ast

    uniquify-locals
    add-binding-atom

    (prewalk #(-> %
                trim-do
                warn-earmuff
                source-info
                elide-meta))

    (postwalk
      (comp (fn [ast]
              (when-let [atom (:atom ast)]
                (swap! atom dissoc :dirty?))
              ast)
            (cycling constant-lift)))

    ((collect {:what       #{:constants
                             :callsites
                             :closed-overs}
               :where      #{:deftype :reify :fn}
               :top-level? false}))


    (prewalk cleanup)
    ))

(defn analyze
  "Returns an AST for the form that's compatible with what tools.emitter.jvm requires.

   Binds tools.analyzer/{macroexpand-1,create-var,parse} to
   tools.analyzer.jvm/{macroexpand-1,create-var,parse} and calls
   tools.analyzer/analyzer on form.

   Calls `run-passes` on the AST."
  [form env]
  (binding [ana/macroexpand-1 macroexpand-1
            ana/create-var    create-var
            ana/parse         parse]
    (run-passes (ana/analyze form env))))
