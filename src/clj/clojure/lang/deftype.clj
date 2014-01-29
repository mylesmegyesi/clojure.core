(ns clojure.lang.deftype
  (:refer-clojure :only [defn reduce concat cons fn let key val]))

(defn expand-methods [methods]
  (reduce
    (fn [acc entry]
      (let [protocol (key entry)]
        (concat (cons protocol (val entry))
                acc)))
    []
    methods))
