(ns clojure.ruby.util)

(defn collase-modules [modules]
  (clojure.string/join "::" modules))

(defn ns->modules [ns-name]
  (map
    clojure.string/capitalize
    (clojure.string/split ns-name #"\.")))

(def namespace->module
  (memoize
    (fn [ns-name]
      (-> ns-name
        ns->modules
        collase-modules))))
