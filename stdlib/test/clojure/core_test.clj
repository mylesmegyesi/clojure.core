(ns clojure.core-test)

; basic equals

;(assert (= 1))
(assert (= 1 1))
(assert (= 1 1 1))
(assert (= 1 1 1 1))
(assert (not (= 1 2)))
(assert (not (= 1 2 1)))
(assert (not (= 1 1 2)))
(assert (not (= 1 1 2 1)))
(assert (not (= 1 1 1 2)))

; basic function that returns its value
;(assert (= 1 ((fn* [x] x) 1)))

; basic function that accepts a function as a param
;((fn* [f]
;   (assert (= 1 (f 1)))
;   (assert (= 2 (f 2))))
;   (fn* [x] x))

; a function that uses variables defined in its enclosing scope
;((fn* [eleven]
;   ((fn* [ten]
;      (assert (= 10 ten))
;      (assert (= 11 eleven))
;      (assert (not (= ten eleven))))
;      10)) 11)
