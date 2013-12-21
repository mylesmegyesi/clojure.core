(ns clojure.ruby.main
  (:refer-clojure :exclude [compile])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.ruby.compiler :refer [compile]]
            [clojure.ruby.analyzer :refer [initial-env]]
            [clojure.java.io       :refer [file]])
  (:import [org.apache.commons.io FilenameUtils FileUtils]))

(defn parse-args [args]
  (loop [[current-arg next-arg & more] args results {}]
    (if (nil? current-arg)
      results
      (cond

        (and next-arg (= current-arg "-I"))
        (recur more (update-in results
                               [:load-path]
                               #(conj (vec %) next-arg)))

        (and next-arg (= current-arg "-o"))
        (recur more (assoc results :output-directory next-arg))

        (and next-arg (= current-arg "-m"))
        (recur more (assoc results :main next-arg))

        :else
        (recur (cons next-arg more)
               (update-in results
                          [:input-directories]
                          #(conj (vec %) current-arg)))))))

(defn- files-in-dir [directory]
  (let [f-dir (file directory)
        abs-dir (str (.getCanonicalPath f-dir) "/")]
    (map
      (fn [f]
        (let [abs (.getCanonicalPath f)]
          {:absolute-path abs
           :relative-path (clojure.string/replace-first abs abs-dir "")}))
      (FileUtils/listFiles f-dir nil true))))

(defn remove-extension [file-path]
  (FilenameUtils/removeExtension file-path))

(defn add-extension [file-path extension]
  (str file-path "." extension))

(defn get-extension [file-path]
  (FilenameUtils/getExtension file-path))

(defn replace-extension [file-path extension]
  (add-extension (remove-extension file-path) extension))

(defn- only-clj [files]
  (filter
    (fn [f]
      (= "clj"
         (get-extension (:absolute-path f))))
    files))

(defn compile-directory [output directory]
  (doseq [{:keys [relative-path absolute-path]} (only-clj (files-in-dir directory))]
    (let [input-file        (file absolute-path)
          output-file-name  (replace-extension relative-path "rb")
          relative-out      (str output "/" output-file-name)
          output-file (file (str (.getCanonicalPath (file output)) "/" output-file-name))]
      (.mkdirs (file (.getParent output-file)))
      (binding [*file* absolute-path]
        (println (format "Compiling %s -> %s" relative-path relative-out))
        (spit
          output-file
          (compile (str "(do" (slurp input-file) ")") (initial-env)))))))

(defn -main [& args]
  (let [{:keys [output-directory input-directories]} (parse-args args)]
    (doseq [input-directory input-directories]
      (compile-directory output-directory input-directory))))
