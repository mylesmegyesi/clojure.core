(ns clojure.lang.callable
  (:refer-clojure :only [defmacro list*]))

(def base-callable Callable)

(defmacro callable-method [bindings & body]
  (list* 'call bindings body))

