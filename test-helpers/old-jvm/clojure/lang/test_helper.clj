(ns clojure.lang.test-helper)

(deftype TenComparator []
  Comparable
  (compareTo [this other] 10))

(defn new-ten-comparator []
  (TenComparator.))

(defn equal-type [equal?]
  (reify Object (equals [this other] equal?)))

(defn equivalent-type [equivalent?]
  (reify Object (equals [this other] equivalent?)))

(defn hashed-type [h]
  (reify Object (hashCode [this] h)))

(defn hashed-and-comparable-type [h i]
  (reify
    Object
    (hashCode [this] h)

    Comparable
    (compareTo [this other] i)))
