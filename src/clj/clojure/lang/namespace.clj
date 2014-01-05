(ns clojure.lang.namespace
  (:refer-clojure :only [defn defn- assoc get-in if-let let assoc-in atom or reduce fn val key -> not select-keys doseq when-not contains? format apply dissoc get set concat keys update-in])
  (:require [clojure.lang.comparison :refer [=]]
            [clojure.lang.named      :refer [namespace name]]
            [clojure.lang.meta       :refer [meta with-meta]]
            [clojure.lang.show       :refer [str]]
            [clojure.lang.symbol     :refer [symbol]]
            [clojure.lang.var        :refer [-alter-var-root make-var make-unbound]]))

(defn create-ns [namespaces ns-sym]
  (assoc namespaces ns-sym {:mappings {}
                            :aliases  {}
                            :ns       ns-sym}))

(defn find-ns [namespaces ns-sym]
  (get-in namespaces [ns-sym :ns]))

(defn the-ns [namespaces namespace-sym]
  (if-let [ns-sym (find-ns namespaces namespace-sym)]
    ns-sym
    (throw (Exception. (str "No namespace: " namespace-sym " found")))))

(def ns-name the-ns)

(defn- resolve-alias [namespaces in-ns-sym alias]
  (get-in namespaces [in-ns-sym :aliases alias]))

(defn alias [namespaces to-ns-sym alias target-ns-sym]
  (let [to-ns (the-ns namespaces to-ns-sym)
        target-ns (the-ns namespaces target-ns-sym)]
    (if-let [existing-alias (resolve-alias namespaces to-ns-sym alias)]
      (if (= existing-alias target-ns-sym)
        namespaces
        (throw (Exception. (str "Alias " alias " already exists in namespace " to-ns ", aliasing " existing-alias))))
      (assoc-in namespaces
                [to-ns :aliases alias]
                target-ns))))

(defn intern [namespaces -ns sym]
  (if (namespace sym)
    (throw (IllegalArgumentException. "Can't intern namespace-qualified symbol")))
  (let [-ns (the-ns namespaces -ns)
        root (atom nil)
        v (make-var (or (meta sym) {}) -ns sym root)]
    (-alter-var-root v (make-unbound v))
    (assoc-in namespaces [-ns :mappings sym] v)))

(defn ns-resolve [namespaces -in-ns sym]
  (let [ns-sym (if-let [ns-part (namespace sym)]
                 (let [ns-sym (symbol ns-part)]
                   (or (resolve-alias namespaces -in-ns ns-sym)
                       ns-sym))
                 (the-ns namespaces -in-ns))]
    (get-in namespaces [ns-sym :mappings (symbol (name sym))])))

(defn -def
  ([namespaces ns-sym sym]
   (intern namespaces ns-sym sym))
  ([namespaces ns-sym sym init]
   (-def namespaces ns-sym sym nil init))
  ([namespaces ns-sym sym doc init]
   (let [sym-meta (assoc (meta sym) :doc doc)
         sym-with-meta (with-meta sym sym-meta)
         new-namespaces (intern namespaces ns-sym sym-with-meta)]
     (-alter-var-root (ns-resolve new-namespaces ns-sym sym-with-meta) init)
     new-namespaces)))

(defn ns-map [namespaces namespace-sym]
  (get-in namespaces [(the-ns namespaces namespace-sym) :mappings]))

(defn- interned-in-ns? [namespace-sym var]
  (= namespace-sym (symbol (namespace var))))

(defn- filter-interned [mappings namespace-sym]
  (reduce
    (fn [acc entry]
      (let [var (val entry)]
        (if (interned-in-ns? namespace-sym var)
          (assoc acc (key entry) (val entry))
          acc)))
    {}
    mappings))

(defn ns-interns [namespaces namespace-sym]
  (-> (ns-map namespaces namespace-sym)
    (filter-interned namespace-sym)))

(defn- private? [var]
  (:private (meta var)))

(defn- filter-private [mappings]
  (reduce
    (fn [acc entry]
      (let [var (val entry)]
        (if (not (private? var))
          (assoc acc (key entry) (val entry))
          acc)))
    {}
    mappings))

(defn ns-publics [namespaces namespace-sym]
  (-> (ns-interns namespaces (the-ns namespaces namespace-sym))
    filter-private))

(defn- filter-not-interned [mappings namespace-sym]
  (reduce
    (fn [acc entry]
      (let [var (val entry)]
        (if (not (interned-in-ns? namespace-sym var))
          (assoc acc (key entry) (val entry))
          acc)))
    {}
    mappings))

(defn ns-refers [namespaces namespace-sym]
  (-> (ns-map namespaces namespace-sym)
    (filter-not-interned namespace-sym)))

(defn ns-aliases [namespaces namespace-sym]
  (get-in namespaces [(the-ns namespaces namespace-sym) :aliases]))

(defn- filter-includes [mappings includes]
  (if includes
    (select-keys mappings includes)
    mappings))

(defn- throw-does-not-exist [sym]
  (throw (Exception. (str sym " does not exist"))))

(defn- validate-exists [mappings namespaces syms message]
  (doseq [sym syms]
    (when-not (contains? mappings sym)
      (throw (Exception. (format message sym)))))
  mappings)

(defn- filter-excludes [mappings excludes]
  (apply dissoc mappings excludes))

(defn- apply-renames [mappings renamings]
  (reduce
    (fn [mappings entry]
      (let [to-rename (key entry)
            new-name (val entry)]
        (assoc (dissoc mappings to-rename)
               new-name (get mappings to-rename))))
    mappings
    renamings))

(defn- add-to-ns [mappings namespaces to-ns]
  (reduce
    (fn [namespaces entry]
      (assoc-in namespaces [to-ns :mappings (key entry)] (val entry)))
    namespaces
    mappings))

(defn refer [namespaces to-ns target-ns & {:keys [exclude only rename] :as filters}]
  (the-ns namespaces to-ns)
  (the-ns namespaces target-ns)
  (let [all-references (set (concat only exclude (keys rename)))]
    (-> (ns-interns namespaces target-ns)
      (validate-exists namespaces all-references "%s does not exist")
      filter-private
      (validate-exists namespaces all-references "%s is not public")
      (filter-includes only)
      (filter-excludes exclude)
      (validate-exists namespaces (keys rename) "%s is not referred")
      (apply-renames rename)
      (add-to-ns namespaces to-ns))))

(defn all-ns [namespaces]
  (keys namespaces))

(defn ns-remove [namespaces namespace-sym]
  (dissoc namespaces namespace-sym))

(defn ns-unmap [namespaces namespace-sym sym]
  (if (namespace sym)
    (throw (Exception. "Can't unintern namespace-qualified symbol")))
  (update-in namespaces
             [(the-ns namespaces namespace-sym) :mappings] #(dissoc % sym)))
