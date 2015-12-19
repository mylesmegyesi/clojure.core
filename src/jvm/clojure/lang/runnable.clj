(ns clojure.lang.runnable
  (:refer-clojure :only [defmacro fn extend-type let list*])
  (:require [clojure.next :refer :all]))

(def base-runnable Runnable)

(defmacro defrunnable [type & body]
  (let [run-fn (reduce
                 (fn [acc item]
                   (if (and (clojure.core/seq? item) (= (first item) '-run))
                     item
                     acc)) body)
        bdy (clojure.core/remove #(= %1 run-fn) body)]
    `(clojure.core/deftype ~type ^{:private true}
       ~@bdy
       Runnable
       (run ~@(rest run-fn)))))

(defmacro runnable-method [bindings & body]
  (list* 'run bindings body))

(defmacro invoke-execute
  ([executor] `(.execute ^Runnable ~executor))
  ([executor arg] `(.execute ^Runnable ~executor ~arg)))

