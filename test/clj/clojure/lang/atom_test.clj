(ns clojure.lang.atom-test
  (:refer-clojure :only [apply assoc fn defmacro defn dorun dotimes first flatten let partition pcalls range repeat vec])
  (:require [clojure.test                         :refer :all]
            [clojure.next                         :refer :all :exclude [first assoc repeat]]
            [clojure.support.exception-assertions :refer [illegal-state-error-is-thrown?]]))

(deftest atom-test
  (testing "creates an atom which can be dereferenced"
    (let [atm (atom "atm")]
      (is (= "atm" (deref atm)))))

  (testing "does not allow an invalid initial state"
    (illegal-state-error-is-thrown? #"Invalid reference state"
      (atom "orange" :validator #(not= "orange" %))))

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

  (testing "compare-and-set! validates the new state"
    (let [atm (atom 2 :validator #(not= 3 %))]
      (illegal-state-error-is-thrown?
        #"Invalid reference state"
        (compare-and-set! atm 2 3))))

  (testing "reset! the atom's state"
    (let [atm (atom "atm")]
      (is (and (= "update" (reset! atm "update"))
               (= "update" (deref atm))))))

  (testing "reset! validates the new state"
    (let [atm (atom 2 :validator #(not= 3 %))]
      (illegal-state-error-is-thrown?
        #"Invalid reference state"
        (reset! atm 3))))

  (testing "swap! the atom's state with a function"
    (let [atm (atom [1, 2])]
      (is (= 1 (swap! atm first)))
      (is (= 1 (deref atm)))))

  (testing "swap! the atom's state with a function and an argument"
    (let [atm (atom 7)]
      (is (= 6 (swap! atm - 1)))
      (is (= 6 (deref atm)))))

  (testing "swap! the atom's state with a function and two arguments"
    (let [atm (atom 7)]
      (is (= 4 (swap! atm - 1 2)))
      (is (= 4 (deref atm)))))

  (testing "swap! the atom's state with a function and arbitrary arguments"
    (let [atm (atom 7)]
      (is (= -3 (swap! atm - 1 2 3 4)))
      (is (= -3 (deref atm)))))

  (testing "swap! validates the new state"
    (let [atm (atom 2 :validator #(not= 3 %))]
      (illegal-state-error-is-thrown?
        #"Invalid reference state"
        (swap! atm inc))))

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
          integer-vecs (vec (clojure.core/map vec (partition vec-size (range distinct-items))))
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
      (is (= (range distinct-items) (clojure.core/sort (flatten (deref atm)))))))

  (defn pinc! [atm nthreads niters]
    (dorun (apply pcalls
                  (repeat nthreads
                          #(dotimes [_ niters]
                             (swap! atm inc))))))

  (testing "swap! inc"
    (let [atm1 (atom 0)
          atm2 (atom 0)]
      (pinc! atm1 10 100)
      (is (= (* 10 100) (deref atm1)))
      (pinc! atm2 20 200)
      (is (= (* 20 200) (deref atm2)))))

  (testing "meta is nil when not defined"
    (let [atm (atom "atm")]
      (is (nil? (meta atm)))))

  (testing "meta is available if defined"
    (let [mta {:foo :bar}
          atm (atom "atm" :meta mta)]
      (is (= mta (meta atm)))))

  (testing "reset-meta! will reset the metadata map"
    (let [atm (atom "atm" :meta {:first :meta})
          reset-meta-value (reset-meta! atm {:second :meta})]
      (is (= {:second :meta} reset-meta-value))
      (is (= {:second :meta} (meta atm)))))

  (testing "alter-meta! will reset the metadata map by applying a function"
    (let [atm (atom "atm" :meta {:first :meta})
          alter-meta-value (alter-meta! atm (fn [_ k v] {k v}) :second :meta)]
      (is (= {:second :meta} alter-meta-value))
      (is (= {:second :meta} (meta atm)))))

  (testing "get-validator will return the current validator function"
    (let [validator-fn #(not= 3 %)
          atm (atom 2 :validator validator-fn)]
      (is (= validator-fn (get-validator atm)))))

  (testing "get-validator is nil if a validator has not been set"
    (let [atm (atom "atm")]
      (is (nil? (get-validator atm)))))

  (testing "set-validator! will set the current validator function"
    (let [validator-fn #(not= 3 %)
          atm (atom 2)]
      (is (nil? (set-validator! atm validator-fn)))
      (is (= validator-fn (get-validator atm)))))

  (testing "add-watch will add a function to be invoked on state changes"
    (let [received-key (atom nil)
          received-atom (atom nil)
          received-old-val (atom nil)
          received-new-val (atom nil)
          watch-fn (fn [k a old nw]
                       (reset! received-key k)
                       (reset! received-atom a)
                       (reset! received-old-val old)
                       (reset! received-new-val nw))
          atm (add-watch (atom "old") :add-watch watch-fn)]
      (reset! atm "new")
      (is (and (= :add-watch (deref received-key))
               (= atm (deref received-atom))
               (= "old" (deref received-old-val))
               (= "new" (deref received-new-val))))))

  (testing "add-watch will overwrite watch-keys"
    (let [received-key (atom nil)
          received-atom (atom nil)
          received-old-val (atom nil)
          received-new-val (atom nil)
          old-watch-fn (fn [k a old nw]
                       (reset! received-key "fail")
                       (reset! received-atom "fail")
                       (reset! received-old-val "fail")
                       (reset! received-new-val "fail"))
          watch-fn (fn [k a old nw]
                       (reset! received-key k)
                       (reset! received-atom a)
                       (reset! received-old-val old)
                       (reset! received-new-val nw))
          tmp-atm (add-watch (atom "old") :add-watch old-watch-fn)
          atm (add-watch tmp-atm :add-watch watch-fn)]
      (reset! atm "new")
      (is (and (= :add-watch (deref received-key))
               (= atm (deref received-atom))
               (= "old" (deref received-old-val))
               (= "new" (deref received-new-val))))))

  (testing "remove-watch will remove a watch key"
    (let [received-key (atom nil)
          received-atom (atom nil)
          received-old-val (atom nil)
          received-new-val (atom nil)
          watch-fn (fn [k a old nw]
                       (reset! received-key k)
                       (reset! received-atom a)
                       (reset! received-old-val old)
                       (reset! received-new-val nw))
          tmp-atm (add-watch (atom "old") :add-watch watch-fn)
          atm (remove-watch tmp-atm :add-watch)]
      (reset! atm "new")
      (is (and (nil? (deref received-key))
               (nil? (deref received-atom))
               (nil? (deref received-old-val))
               (nil? (deref received-new-val))))))

  )

(deftest memoize-test
  (testing "produces a wrapper which passes through"
    (is (= 42 ((memoize (fn [] 42))))))

  (testing "memoizes the result of function calls"
    (let [memoized-fn (memoize (fn [_] (rand-int 100)))]
      (is (= (memoized-fn :foo) (memoized-fn :foo)))))

  )
