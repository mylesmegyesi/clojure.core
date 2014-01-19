(ns leiningen.clr-test
  (:require [leiningen.core.project :as project]
            [leiningen.test         :as lein-test]
            [leiningen.clr.internal :as in]
            [leiningen.clr          :as lein-clr]))

(defn form-for-testing-namespaces
  "Return a form that when eval'd in the context of the project will test
  each namespace and print an overall summary."
  ([namespaces _ & [selectors]]
     (let [ns-sym (gensym "namespaces")]
       `(let [~ns-sym ~(#'lein-test/form-for-select-namespaces namespaces selectors)]
          (when (seq ~ns-sym)
            (apply require :reload ~ns-sym))
          (let [failures# (atom #{})
                selected-namespaces# ~(#'lein-test/form-for-nses-selectors-match selectors ns-sym)
                summary# (binding [clojure.test/*test-out* *out*]
                           (~lein-test/form-for-suppressing-unselected-tests
                            selected-namespaces# ~selectors
                            #(apply ~'clojure.test/run-tests selected-namespaces#)))]
            (Environment/Exit (+ (:error summary#) (:fail summary#))))))))

(defn clr-test
  [project & tests]
  (let [init-file (lein-clr/asm-load-init project (in/get-temp-file))
        allp (lein-clr/all-load-paths project)
        project (project/merge-profiles project [:leiningen/test :test])
        [nses selectors] (#'lein-test/read-args tests project)
        form (form-for-testing-namespaces nses nil (vec selectors))
        expr (pr-str (list 'do (list 'require ''clojure.test) form))
        exec (concat
               (lein-clr/clj-main-cmd project)
               ["-i" init-file "-e" (str (lein-clr/get-eval-string project) expr)])]
    (println "Running ClojureCLR tests")
    (in/with-process-builder
      pb (:root project) exec
      (in/configure-load-path (.environment ^ProcessBuilder pb) allp)
      (apply in/verbose "Running: " (map pr-str exec))
      (in/run-process pb))))
