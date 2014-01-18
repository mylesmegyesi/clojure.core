(ns clojure.lang.test-helper)

(deftype TenComparator []
  IComparable
  (CompareTo [this other] 10))

(defn new-ten-comparator []
  (TenComparator.))

(defn equal-type [equal?]
  (reify Object (Equals [this other] equal?)))

(defn equivalent-type [equivalent?]
  (reify Object (Equals [this other] equivalent?)))

(defn hashed-type [h]
  (reify Object (GetHashCode [this] h)))

(defn hashed-and-comparable-type [h i]
  (reify
    Object
    (GetHashCode [this] h)

    IComparable
    (CompareTo [this other] i)))
