(ns clojure.lang.delay-test
  (:refer-clojure :only [let fn pmap loop list])
  (:require [clojure.test :refer :all]
            [clojure.next :refer :all]))

(deftest delay-test
  (testing "creates a delay"
    (let [dly (delay "foo")]
      (is (= "foo" (deref dly)))))

  (testing "returns the cached value on subsequent calls"
    (let [one (atom 0)
          dly (delay (swap! one inc) "done")]
      (is (= "done" (deref dly)))
      (is (= "done" (deref dly)))
      (is (= 1 (deref one)))))

  (testing "knows if its realized"
    (let [dly (delay (+ 1 2))]
      (is (not (realized? dly)))
      (deref dly)
      (is (realized? dly))))

  (testing "derefs in a synchronized operation"
    (let [nthreads (atom 0)
          one (atom 0)
          dly (delay
                (loop []
                  (if (= (deref nthreads) 3)
                    (swap! one inc)
                    (recur))))
          derefs (pmap
                   (fn [d]
                     (swap! nthreads inc)
                     (list (realized? d) (deref d)))
                   (list dly dly dly))
          realized-list (clojure.core/map first derefs)
          deref-list (map last derefs)]
      (is (= 2 (count (seq (filter identity realized-list)))))
      (is (= 1 (nth deref-list 0)))
      (is (= 1 (nth deref-list 1)))
      (is (= 1 (nth deref-list 2)))))

  (testing "force derefs the delay"
    (is (= 5 (force (delay (- 10 5))))))

  (testing "force returns the object if it is not a delay"
    (is (= 10 (force 10))))

  (testing "delay? returns true for a delay"
    (is (delay? (delay "foo"))))

  (testing "delay? retrusn false when not a delay"
    (is (not (delay? "foo"))))
)
