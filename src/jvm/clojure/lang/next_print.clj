(in-ns 'clojure.next)

(defmethod print-method :default [o ^java.io.Writer w]
  (print-simple o w))
