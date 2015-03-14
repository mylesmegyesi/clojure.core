(ns clojure.lang.input-output
  (:refer-clojure :only [class defmacro let])
  (:require [clojure.next :refer :all])
  (:import [java.io Writer OutputStreamWriter StringWriter]))

(defmacro platform-print-constructor [obj print-args wrtr]
  `(do
     (.write ~wrtr "#=(")
     (.write ~wrtr (.getName ^Class (class ~obj)))
     (.write ~wrtr ". ")
     (~print-args ~obj ~wrtr)
     (.write ~wrtr ")")))

(defmacro default-out []
  (OutputStreamWriter. (. System out)))

(defmacro platform-newline []
  (let [separator (System/getProperty "line.separator")]
    `(. clojure.next/*out* (append ~separator))))

(defmacro platform-flush []
  `(. clojure.next/*out* flush))

(defmacro platform-write [wrtr obj]
  `(.write ~wrtr ~obj))

(defmacro platform-append-space [wrtr]
  `(.append ~wrtr \space))

(defmacro platform-out-str []
  `(new StringWriter))

