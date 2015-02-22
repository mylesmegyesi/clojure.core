(ns clojure.lang.input-output
  (:refer-clojure :only [defmacro defn let pos? when when-let])
  (:require [clojure.next :refer :all])
  (:import [java.io Writer OutputStreamWriter StringWriter]))

(defmacro default-out []
  (OutputStreamWriter. (. System out)))

(defmacro platform-newline []
  (let [separator (System/getProperty "line.separator")]
    `(. clojure.next/*out* (append ~separator))))

(defmacro platform-flush []
  `(. clojure.next/*out* flush))

(defmacro platform-write [w o]
  `(.write ~w ~o))

(defmacro platform-out-str []
  `(new StringWriter))

(defn print-meta [o ^Writer w]
  (when-let [m (meta o)]
    (when (and (pos? (count m))
            (or *print-dup*
              (and *print-meta* *print-readably*)))
      (.write w "^")
      (if (and (= (count m) 1) (:tag m))
          (pr (:tag m) w)
          (pr m w))
      (.write w " "))))

