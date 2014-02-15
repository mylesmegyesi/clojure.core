(ns clojure.lang.exceptions)

(def out-of-bounds-exception ArrayIndexOutOfBoundsException)

(defmacro new-out-of-bounds-exception [& args]
  (list* 'new out-of-bounds-exception args))
