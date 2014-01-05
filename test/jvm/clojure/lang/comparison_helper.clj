(ns clojure.lang.comparison-helper
  (:refer-clojure :only [deftype defn]))

(deftype Apple [equals-fn compare-fn]
  Object
  (equals [this other] (equals-fn this other))

  Comparable
  (compareTo [this other] (compare-fn this other))

  )

(deftype Orange [equals-fn compare-fn]
  Object
  (equals [this other] (equals-fn this other))

  Comparable
  (compareTo [this other] (compare-fn this other))

  )

(defn new-apple [equals-fn compare-fn]
  (Apple. equals-fn compare-fn))

(defn new-orange [equals-fn compare-fn]
  (Orange. equals-fn compare-fn))
