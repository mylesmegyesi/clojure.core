(ns clojure.lang.sequence
  (:refer-clojure :only [cond declare defn defn- if-let let])
  (:require [clojure.next                 :refer :all]
            [clojure.lang.aseq            :refer [defseq]]
            [clojure.lang.persistent-list :refer [EMPTY-LIST]]
            [clojure.lang.protocols       :refer [IIndexedSeq IPersistentCollection
                                                  ICounted IObj IMeta
                                                  ISeq]]))

(declare make-string-seq)

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
    (if-let [s (next this)] s EMPTY-LIST))


  )

(defn- make-string-seq
  ([s]
    (make-string-seq s 0 nil))
  ([^CharSequence s i mta]
    (if (zero? (.length s))
      nil
      (StringSeq. s i mta))))

(defn platform-seq [s]
  (cond
    (instance? CharSequence s)
      (make-string-seq s)
    :else
      (let [c (.getClass s)]
        (throw
          (IllegalArgumentException.
            (str "Don't know how to create ISeq from: " (.getName c)))))))
