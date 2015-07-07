(ns clojure.lang.input-output
  (:refer-clojure :only [defmacro defmethod let])
  (:require [clojure.next :refer :all])
  (:import [java.io Writer OutputStreamWriter StringWriter]
           [clojure.next]))

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

(defmethod print-method java.lang.Number [n ^Writer wrtr]
  (.write wrtr (str n)))

(defmethod print-method java.math.BigDecimal [n ^Writer wrtr]
  (.write wrtr (str n))
  (.write wrtr "M"))

(defmethod print-method clojure.lang.BigInt [n ^Writer wrtr]
  (.write wrtr (str n))
  (.write wrtr "N"))

(defmethod print-method java.lang.Class [^Class c ^Writer wrtr]
  (.write wrtr (.getName c)))

