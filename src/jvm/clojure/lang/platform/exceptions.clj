(ns clojure.lang.platform.exceptions)

(def argument-error IllegalArgumentException)

(defmacro new-argument-error [& args]
  (list* 'new argument-error args))
