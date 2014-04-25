(ns clojure.lang.platform.exceptions)

(def argument-error IllegalArgumentException)

(defmacro new-argument-error [& args]
  (list* 'new argument-error args))

(def illegal-state-error IllegalStateException)

(defmacro new-illegal-state-error [& args]
  (list* 'new illegal-state-error args))

(def unsupported-error UnsupportedOperationException)

(defmacro new-unsupported-error [& args]
  (list* 'new unsupported-error args))

(def out-of-bounds-exception ArrayIndexOutOfBoundsException)

(defmacro new-out-of-bounds-exception [& args]
  (list* 'new out-of-bounds-exception args))

(def class-cast-exception ClassCastException)

(defmacro new-class-cast-exception [& args]
  (list* 'new class-cast-exception args))
