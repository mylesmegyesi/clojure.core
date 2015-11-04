(ns clojure.lang.input-output
  (:refer-clojure :only [binding cond defmacro defmethod defn dotimes fn let loop neg? pos? prefer-method satisfies? when when-let])
  (:require [clojure.next           :refer :all]
            [clojure.lang.protocols :refer [IMeta]])
  (:import [java.io Writer OutputStreamWriter StringWriter]
           [clojure.next]))

(defn platform-read-line [^java.io.BufferedReader rdr]
  (.readLine rdr))

(defn platform-print-constructor [obj print-args ^Writer wrtr]
  (.write wrtr "#=(")
  (.write wrtr (.getName ^Class (class obj)))
  (.write wrtr ". ")
  (print-args obj wrtr)
  (.write wrtr ")"))

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

(defn print-sequential [begin print-one sep end sequence wrtr]
  (binding [*print-level* (and (not *print-dup*) *print-level* (dec *print-level*))]
    (if (and *print-level* (neg? *print-level*))
      (platform-write wrtr "#")
      (do
        (platform-write wrtr begin)
        (when-let [xs (seq sequence)]
          (if (and (not *print-dup*) *print-length*)
            ; TODO: switch to [[x & xs] xs] once
            ; it is using clojure.core/nth
            (loop [rxs xs
                   print-length *print-length*]
              (let [x (first rxs)
                    xs (next rxs)]
                (if (zero? print-length)
                  (platform-write wrtr "...")
                  (do
                    (print-one x wrtr)
                    (when xs
                      (platform-write wrtr sep)
                      (recur xs (dec print-length)))))))
            ; TODO: switch to [[x & xs] xs] once
            ; it is using clojure.core/nth
            (loop [rxs xs]
              (let [x (first rxs)
                    xs (next rxs)]
                (print-one x wrtr)
                (when xs
                  (platform-write wrtr sep)
                  (recur xs))))))
        (platform-write wrtr end)))))

(defn print-map [m pr-on wrtr]
  (print-sequential "{"
    (fn [e wrtr]
      (do
        (pr-on (key e) wrtr)
        (platform-append-space wrtr)
        (pr-on (val e) wrtr)))
    ", " "}" (seq m) wrtr))

(defn print-meta [o ^Writer wrtr]
  (when-let [m (meta o)]
    (when (and (pos? (count m))
            (or *print-dup*
              (and *print-meta* *print-readably*)))
      (.write wrtr "^")
      (if (and (= (count m) 1) (:tag m))
          (print-method (:tag m) wrtr)
          (print-method m wrtr))
      (.write wrtr " "))))

(defn print-object [o ^Writer wrtr]
  (print-meta o wrtr)
  (.write wrtr "#<")
  (let [n (.getSimpleName ^Class (class o))]
    (when (not= n "")
      (.write wrtr n)
      (.write wrtr " ")))
  (.write wrtr (str o))
  (.write wrtr ">"))

(defmethod print-method java.lang.Object [o w]
  (print-object o w))

(defmethod print-method java.lang.Number [n ^Writer wrtr]
  (.write wrtr (str n)))

(defmethod print-method java.lang.String [^String s ^Writer wrtr]
  (if (or *print-dup* *print-readably*)
    (do
      (.append wrtr \")
      (dotimes [n (count s)]
        (let [c (.charAt s n)
              e (get char-escape-string c)]
          (if e (.write wrtr e) (.append wrtr c))))
      (.append wrtr \"))
    (.write wrtr s))
  nil)

(defmethod print-method java.lang.Character [c ^Writer wrtr]
  (if (or *print-dup* *print-readably*)
    (do
      (.append wrtr \\)
      (let [n (get char-name-string c)]
        (if n
          (.write wrtr n)
          (.append wrtr c))))
      (.append wrtr c))
  nil)

(defmethod print-method java.math.BigDecimal [n ^Writer wrtr]
  (.write wrtr (str n))
  (.write wrtr "M"))

(defmethod print-method clojure.lang.platform.BigInt [n ^Writer wrtr]
  (.write wrtr (str n))
  (.write wrtr "N"))

(defmethod print-method java.lang.Class [^Class c ^Writer wrtr]
  (.write wrtr (.getName c)))

(defmethod print-method java.lang.Boolean [b ^Writer wrtr]
  (.write wrtr (str b)))

(defmethod print-method java.util.List [l wrtr]
  (if *print-readably*
    (do
      (print-meta l wrtr)
      (print-sequential "(" print-method " " ")" l wrtr))
    (print-object l wrtr)))

(defmethod print-method java.util.RandomAccess [ra wrtr]
  (if *print-readably*
    (do
      (print-meta ra wrtr)
      (print-sequential "[" print-method " " "]" ra wrtr))
    (print-object ra wrtr)))

(defmethod print-method java.util.Map [m wrtr]
  (if *print-readably*
    (do
      (print-meta m wrtr)
      (print-map m print-method wrtr))
    (print-object m wrtr)))

(defmethod print-method java.util.Set [s wrtr]
  (if *print-readably*
    (do
      (print-meta s wrtr)
      (print-sequential "#{" print-method " " "}" (seq s) wrtr))
    (print-object s wrtr)))

(defmethod print-method java.util.regex.Pattern [^java.util.regex.Pattern p ^Writer wrtr]
  (.write wrtr "#\"")
  (loop [s (seq (.pattern p))
         qmode false]
    (let [^Character c (first s)
          r (next s)]
      (when s
        (cond
          (= c \\) (let [^Character c2 (first r)
                         r2 (rest r)]
                     (.append wrtr \\)
                     (.append wrtr c2)
                     (if qmode
                        (recur r2 (not= c2 \E))
                        (recur r2 (= c2 \Q))))
          (= c \") (do
                     (if qmode
                       (.write wrtr "\\E\\\"\\Q")
                       (.write wrtr "\\\""))
                     (recur r qmode))
          :else    (do
                     (.append wrtr c)
                     (recur r qmode))))))
  (.append wrtr \"))

(prefer-method print-method java.util.RandomAccess java.util.List)

(defmethod print-dup java.lang.Number [n wrtr]
  (print-ctor n (fn [o w]
                  (print-dup (str o) w)) wrtr))

(defmethod print-dup java.lang.String [s wrtr]
  (print-method s wrtr))

(defmethod print-dup java.lang.Boolean [b wrtr]
  (print-method b wrtr))

