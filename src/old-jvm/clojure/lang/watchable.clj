(ns clojure.lang.watchable
  (:refer-clojure :only [add-watch remove-watch]
                  :rename {add-watch core-add-watch
                           remove-watch core-remove-watch
                           }))

(def add-watch core-add-watch)
(def remove-watch core-remove-watch)
