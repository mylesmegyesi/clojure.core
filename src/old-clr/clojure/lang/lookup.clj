(ns clojure.lang.lookup
  (:refer-clojure :only [contains? get]
                  :rename {contains? core-contains?
                           get core-get}))

(def contains? core-contains?)
(def get core-get)
