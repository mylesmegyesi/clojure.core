(ns clojure.lang.atom-test
  (:refer-clojure :only [str and apply assoc fn defn dorun dotimes first flatten let map nth partition pcalls range rand-int repeat sort vec - /])
  (:require [clojure.test             :refer :all]
            [clojure.lang.atom        :refer :all]
            [clojure.lang.deref       :refer [deref]]
            [clojure.lang.equivalence :refer [not not= =]]))

(deftest atom-test
  (testing "creates an atom which can be dereferenced"
    (let [atm (atom "atm")]
      (is (= "atm" (deref atm)))))

  (testing "allows an atom's state to be set if the current state's comparison succeeds"
    (let [atm     (atom "atm")
          success (compare-and-set! atm "atm" "new atm")]
      (is (and success
               (= "new atm" (deref atm))))))

  (testing "does not allow an atom's state to be set if the current state's comparison fails"
    (let [atm     (atom "atm")
          success (compare-and-set! atm "not atm" "new atm")]
      (is (and (not success)
               (= "atm" (deref atm))))))

  (testing "reset! the atom's state"
    (let [atm (atom "atm")]
      (is (and (= "update" (reset! atm "update"))
               (= "update" (deref atm))))))

  (testing "swap! the atom's state with a function"
    (let [atm (atom [1, 2])]
      (is (and (= 1 (swap! atm first))
               (= 1 (deref atm))))))

  (testing "swap! the atom's state with a function and an argument"
    (let [atm (atom 7)]
      (is (and (= 6 (swap! atm - 1))
               (= 6 (deref atm))))))

  (testing "swap! the atom's state with a function and two arguments"
    (let [atm (atom 7)]
      (is (and (= 4 (swap! atm - 1 2))
               (= 4 (deref atm))))))

  (testing "swap! the atom's state with a function and arbitrary arguments"
    (let [atm (atom 7)]
      (is (and (= -3 (swap! atm - 1 2 3 4))
               (= -3 (deref atm))))))

  (defn swap-items-in [atm v1 v2 i1 i2]
    (swap! atm
      (fn [integer-vecs]
        (let [temp ((integer-vecs v1) i1)
              updated-integer-vecs (assoc integer-vecs v1
                                     (assoc (integer-vecs v1) i1 ((integer-vecs v2) i2)))]
          (assoc updated-integer-vecs v2
            (assoc (updated-integer-vecs v2) i2 temp))))))

  (testing "swap! is an atomic operation"
    (let [distinct-items 50
          vec-size 10
          integer-vecs (vec (map vec (partition vec-size (range distinct-items))))
          atm (atom integer-vecs)
          thread-count 10
          iterations-per-thread 100]
      (dorun
        (apply pcalls
          (repeat thread-count
            #(dotimes [_ iterations-per-thread]
              (let [v1 (rand-int (/ distinct-items vec-size))
                    v2 (rand-int (/ distinct-items vec-size))
                    i1 (rand-int vec-size)
                    i2 (rand-int vec-size)]
                (swap-items-in atm v1 v2 i1 i2))))))
      (is (= (range distinct-items) (sort (flatten (deref atm)))))))
  )
