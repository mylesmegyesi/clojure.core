(ns clojure.ruby.main
  (:refer-clojure :exclude [compile])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.ruby.compiler :refer [compile]]
            [clojure.java.io       :refer [file]]))

(defn parse-args [args]
  (loop [[current-arg next-arg & more] args results {}]
    (if (nil? current-arg)
      results
      (cond

        (and next-arg (= current-arg "-I"))
        (recur more (update-in results
                               [:search-paths]
                               #(conj (vec %) next-arg)))

        (and next-arg (= current-arg "-o"))
        (recur more (assoc results :output-file next-arg))

        (and next-arg (= current-arg "-m"))
        (recur more (assoc results :main next-arg))

        :else
        (recur (cons next-arg more)
               (update-in results
                          [:input-filenames]
                          #(conj (vec %) current-arg)))))))

(def empty-env
  {:context :expr
   :locals {}
   :namespaces (atom {})})

(defn compile-input-files [{:keys [input-filenames]}]
  (for [input-filename input-filenames]
    (let [input-file    (file input-filename)
          absolute-path (.getCanonicalPath input-file)]
      (binding [*file* absolute-path]
        (compile (str "(do " (slurp input-filename) ")") empty-env)))))

(defn -main [& args]
  (let [options (parse-args args)]
    (spit
      (:output-file options)
      (clojure.string/join "\n"
                           (compile-input-files options)))))
