
(let* [keyword-type  (create-type* Keyword [name])
       make-keyword  (fn* [name] (new keyword-type name))

       hash-map-type (create-type* PersistentHashMap [things])
       make-map      (fn* [& things] (new hash-map-type things))

       symbol-type   (create-type* Symbol [name])
       make-sym      (fn* [name] (new symbol-type name))

       -create-ns (fn* [namespaces ns-sym]
                    (-assoc namespaces ns-sym (make-map (make-keyword "mappings") (make-map)
                                                        (make-keyword "aliases")  (make-map)
                                                        (make-keyword "ns")       ns-sym)))
       namespaces (make-map)
       namespaces (-create-ns namespaces (make-sym "clojure.core"))]
  namespaces)
