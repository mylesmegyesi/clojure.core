(ns clojure.lang.keyword
  (:refer-clojure :only [keyword keyword?]
                  :rename {keyword core-keyword
                           keyword? core-keyword?}))

(def keyword core-keyword)
(def keyword? core-keyword?)
