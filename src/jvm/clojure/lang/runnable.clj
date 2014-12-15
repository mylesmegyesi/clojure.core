(ns clojure.lang.runnable
  (:refer-clojure :only [defmacro fn extend-type let reduce remove seq?])
  (:require [clojure.next :refer :all :except [reduce]]))

(defmacro defrunnable [type & body]
  (let [run-fn (reduce
                 (fn [acc item]
                   (if (and (seq? item) (= (first item) '-run))
                     item
                     acc)) body)
        bdy (remove #(= %1 run-fn) body)]
    `(clojure.core/deftype ~type ^{:private true}
       ~@bdy
       Runnable
       (run ~@(rest run-fn)))))

(defmacro invoke-execute
  ([executor] `(.execute ~executor))
  ([executor arg] `(.execute ~executor ~arg)))

