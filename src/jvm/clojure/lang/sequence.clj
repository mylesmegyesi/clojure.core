(ns clojure.lang.sequence
  (:refer-clojure :only [cond declare defn defn- if-let let locking loop satisfies? when])
  (:require [clojure.next                 :refer :all]
            [clojure.lang.aseq            :refer [defseq]]
            [clojure.lang.persistent-list :refer [EMPTY-LIST]]
            [clojure.lang.protocols       :refer [IIndexedSeq IPersistentCollection
                                                  ICounted IObj IMeta
                                                  ISeq]])
  (:import [java.lang.reflect Array]
           [java.util Iterator Map]))

(declare make-iterator-seq
         make-array-seq
         make-string-seq)

(defseq ^:private IteratorSeq [-iter
                               ^:volatile-mutable -val
                               ^:volatile-mutable -rest
                               -state-obj
                               -meta]
  ICounted
  (-count [this]
    (loop [i 1
           sq (next this)]
      (if sq
        (if (and (satisfies? ICounted sq) (not (instance? IteratorSeq sq)))
          (recur (inc (+ i (count sq))) (next sq))
          (recur (inc i) (next sq)))
        i)))

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (make-iterator-seq -iter -val -rest -state-obj m))

  IPersistentCollection
  (-cons [this x]
    (cons x this))

  (-empty [this] EMPTY-LIST)

  ISeq
  (-first [this]
    (if (= -val -state-obj)
      (locking -state-obj
        (when (= -val -state-obj)
          (set! -val (.next ^Iterator -iter))
          -val))
      -val))

  (-next [this]
    (if (= -rest -state-obj)
      (locking -state-obj
        (when (= -rest -state-obj)
          (first this)
          (set! -rest (make-iterator-seq -iter))
          -rest))
      -rest))

  (-more [this]
    (if-let [s (next this)] s EMPTY-LIST)))

(defn- make-iterator-seq
  ([iter]
   (let [state-obj (Object.)]
     (if (.hasNext iter)
       (make-iterator-seq iter state-obj state-obj state-obj nil)
       nil)))
  ([^Iterator iter v r state-obj mta]
    (IteratorSeq. iter v r state-obj mta)))

(defseq ^:private ArraySeq [-arr -i -meta]
  ICounted
  (-count [this]
    (if -arr
      (- (Array/getLength -arr) -i)
      0))

  IIndexedSeq
  (-index [this] -i)

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (if (= m -meta)
      this
      (make-array-seq -arr -i m)))

  IPersistentCollection
  (-cons [this x]
    (cons x this))

  (-empty [this] EMPTY-LIST)

  ISeq
  (-first [this]
    (if -arr
      (Array/get -arr -i)
      nil))

  (-next [this]
    (if (and -arr (< (inc -i) (Array/getLength -arr)))
      (make-array-seq -arr (inc -i) -meta)))

  (-more [this]
    (if-let [s (next this)] s EMPTY-LIST)))

(defn- make-array-seq
  ([arr]
    (make-array-seq arr 0 nil))
  ([arr i mta]
    (ArraySeq. arr i mta)))

(defseq ^:private StringSeq [-string -i -meta]
  ICounted
  (-count [this]
    (- (.length -string) -i))

  IIndexedSeq
  (-index [this] -i)

  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this m]
    (if (= m -meta)
      this
      (make-string-seq -string -i m)))

  IPersistentCollection
  (-cons [this x]
    (cons x this))

  (-empty [this] EMPTY-LIST)

  ISeq
  (-first [this]
    (let [chr (.charAt -string -i)]
      (. Character (valueOf chr))))

  (-next [this]
    (if (< (inc -i) (.length -string))
      (make-string-seq -string (inc -i) -meta)
      nil))

  (-more [this]
    (if-let [s (next this)] s EMPTY-LIST)))

(defn- make-string-seq
  ([s]
    (make-string-seq s 0 nil))
  ([^CharSequence s i mta]
    (if (zero? (.length s))
      nil
      (StringSeq. s i mta))))

(defn platform-seq [s]
  (cond
    (instance? Iterable s)
      (make-iterator-seq (.iterator ^Iterator s))
    (.isArray ^Class (class s))
      (make-array-seq s)
    (instance? CharSequence s)
      (make-string-seq s)
    (instance? Map s)
      (seq (.entrySet ^Map s))
    :else
      (let [c (.getClass s)]
        (throw
          (IllegalArgumentException.
            (str "Don't know how to create ISeq from: " (.getName ^Class c)))))))

