(ns clojure.lang.seqable
  (:refer-clojure :only [seq]
                  :rename {seq core-seq}))

(def seq core-seq)
