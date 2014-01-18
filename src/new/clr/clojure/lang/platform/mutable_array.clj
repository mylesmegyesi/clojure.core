(ns clojure.lang.platform.mutable-array
  (:refer-clojure :only [defn let loop inc int])
  (:import [System.Collections ArrayList]))

(defn make-array [size]
  (.ToArray (ArrayList. (int size))))

(defn make-array-with-items [enumerable]
  (let [enumerator (.GetEnumerator enumerable)
        ;_ (clojure.core/prn enumerator)
        arr-list (ArrayList.)]
    (loop [has-next? (.MoveNext enumerator)
           size 0]
      (if has-next?
        (do
          (.Add arr-list (.Current enumerator))
          (recur (.MoveNext enumerator) (inc size)))
        [(.ToArray arr-list) size]))))

(defn array-set! [arr idx v]
  (.SetValue arr v idx))

(defn array-get [arr idx]
  (.GetValue arr idx))

(defn array-copy! [src src-pos dest dest-pos length]
  (Array/Copy src src-pos dest dest-pos length))
