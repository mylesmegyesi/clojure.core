(ns clojure.lang.time
  (:refer-clojure :only [defn]))

(defn nano-time []
  (. System nanoTime))
