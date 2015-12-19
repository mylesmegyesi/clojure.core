(ns clojure.lang.exceptions
  (:import [clojure.lang.platform ArityException]))

(def argument-error IllegalArgumentException)

(defmacro new-argument-error [& args]
  (list* 'new argument-error args))

(def arithmetic-exception ArithmeticException)

(defmacro new-arithmetic-exception [& args]
  (list* 'new arithmetic-exception args))

(def arity-exception ArityException)

(defmacro new-arity-exception [& args]
  (list* 'new arity-exception args))

(def assertion-error AssertionError)

(defmacro new-assertion-error [& args]
  (list* 'new assertion-error args))

(def illegal-access-error IllegalAccessError)

(defmacro new-illegal-access-error [& args]
  (list* 'new illegal-access-error args))

(def illegal-state-error IllegalStateException)

(defmacro new-illegal-state-error [& args]
  (list* 'new illegal-state-error args))

(def unsupported-error UnsupportedOperationException)

(defmacro new-unsupported-error [& args]
  (if (nil? args)
    (list 'new unsupported-error)
    (list* 'new unsupported-error args)))

(def out-of-bounds-exception IndexOutOfBoundsException)

(defmacro new-out-of-bounds-exception [& args]
  (list* 'new out-of-bounds-exception args))

(def class-cast-exception ClassCastException)

(defmacro new-class-cast-exception [& args]
  (list* 'new class-cast-exception args))

(def runtime-exception RuntimeException)

(defmacro new-runtime-exception [& args]
  (list* 'new runtime-exception args))

(def exception Exception)

(defmacro new-exception [& args]
  (list* 'new exception args))

(def throwable Throwable)

; Try is a special form which won't resolve the classname
; so we can't simply say
; (try ... (catch clojure.lang.platform.exceptions/throwable error ...))
(defmacro platform-try [& body]
  (let [c (reduce (fn [acc item]
                    (if (and (seq? item)
                             (= 'platform-catch (first item)))
                      (rest item)
                      acc))
                  nil body)
        exception-class (eval (first c))
        exception-var (first (rest c))
        catch-block (rest (rest c))
        try-block (take-while (fn [item]
                                (if (seq? item)
                                  (not= (first item) 'platform-catch)
                                  true)) body)]
    `(try ~@try-block
       (catch ~exception-class ~exception-var ~@catch-block))))

