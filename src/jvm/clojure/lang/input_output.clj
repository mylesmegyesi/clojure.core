(ns clojure.lang.input-output
  (:refer-clojure :only [defmacro let])
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

(defmacro platform-append-space [w]
  `(.append ~w \space))

(defmacro platform-out-str []
  `(new StringWriter))

