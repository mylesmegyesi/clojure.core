(ns clojure.lang.namespace
  (:refer-clojure :only [defn str]))

(defn create-ns [namespaces ns-sym]
  namespaces)

(defn find-ns [namespaces ns-sym]
  )

(defn the-ns [namespaces ns-sym]
  (throw (Exception. (str "No namespace: " ns-sym " found"))))

(def ns-name the-ns)

(defn alias [namespaces to-ns-sym alias target-ns-sym]
  namespaces)

(defn intern [namespaces ns sym]
  namespaces)

(defn ns-resolve [namespaces in-ns sym]
  )

(defn -def
  ([namespaces ns-sym sym]
   (intern namespaces ns-sym sym))
  ([namespaces ns-sym sym init]
   (-def namespaces ns-sym sym nil init))
  ([namespaces ns-sym sym doc init]
   namespaces))

(defn ns-map [namespaces ns-sym]
  {})

(defn ns-interns [namespaces ns-sym]
  {})

(defn ns-publics [namespaces ns-sym]
  {})

(defn ns-refers [namespaces ns-sym]
  {})

(defn ns-aliases [namespaces ns-sym]
  {})

(defn refer [namespaces to-ns target-ns & filters]
  namespaces)

(defn all-ns [namespaces]
  [])

(defn ns-remove [namespaces ns-sym]
  namespaces)

(defn ns-unmap [namespaces ns-sym sym]
  namespaces)
