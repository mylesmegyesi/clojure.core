(ns clojure.lang.size
  (:refer-clojure :only [cond defn let])
  (:require [clojure.next :refer :all])
  (:import [java.util Collection Map]
           [java.lang.reflect Array]))

(defn platform-count [obj]
  (cond
    (instance? CharSequence obj)
      (.length ^CharSequence obj)
    (instance? Collection obj)
      (.size ^Collection obj)
    (instance? Map obj)
      (.size ^Map obj)
    (.isArray ^Class (.getClass ^Object obj))
      (. Array getLength obj)
    :else
      (let [msg (str "count not supported on this type: "
                     (.getSimpleName ^Class (.getClass ^Object obj)))]
        (throw (UnsupportedOperationException. ^String msg)))))
