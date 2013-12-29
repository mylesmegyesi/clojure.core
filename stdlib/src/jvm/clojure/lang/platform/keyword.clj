(ns clojure.lang.platform.keyword
  (:refer-clojure :only [defn list]))

(defn platform-keyword-methods []
  ['Object
   (list 'toString ['this] 'str)
   (list 'hashCode ['this] 'hash)
   (list 'equals   ['this 'other]
         (list '== 'sym 'other))

   'Comparable
   (list 'compareTo ['this 'other]
         (list 'compare 'sym 'other))])
