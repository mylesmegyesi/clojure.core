(ns clojure.lang.show
  (:refer-clojure :only [str]
                  :rename {str core-str}))

(def str core-str)
