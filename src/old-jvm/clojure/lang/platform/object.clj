(ns clojure.lang.platform.object
  (:refer-clojure :only [identical? type]
                  :rename {identical? core-identical?
                           type core-type}))

(def identical? core-identical?)
(def type core-type)
