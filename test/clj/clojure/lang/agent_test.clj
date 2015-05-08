(ns clojure.lang.agent-test
  (:refer-clojure :only [class defmacro false? fn let list list* nil? true?])
  (:require [clojure.test            :refer :all]
            [clojure.next            :refer :all]
            [clojure.lang.thread     :refer [sleep]]
            [clojure.lang.exceptions :refer [argument-error new-runtime-exception runtime-exception]]))

(defmacro runtime-exception-is-thrown? [msg & body]
  (list 'is (list* 'thrown-with-msg? runtime-exception msg body)))

(deftest agent-test
  (testing "creates an agent which can be deferenced"
    (let [agt (agent "agt")]
      (is (= "agt" (deref agt)))))

  (testing "send-via the solo-executor"
    (let [agt (agent 0)]
      (send-via clojure.lang.agent/solo-executor agt + 1 1)
      (await agt)
      (is (= 2 (deref agt)))))

  (testing "send-off"
    (let [agt (agent 0)]
      (send-off agt + 1 1)
      (await agt)
      (is (= 2 (deref agt)))))

  (testing "send-via the pooled-executor"
    (let [agt (agent 0)]
      (send-via clojure.lang.agent/pooled-executor agt + 1 1)
      (await agt)
      (is (= 2 (deref agt)))))

  (testing "send"
    (let [agt (agent 0)]
      (send agt + 1 1)
      (await agt)
      (is (= 2 (deref agt)))))

  (testing "an agent's error is nil by default"
    (is (nil? (agent-error (agent 0)))))

  (testing "an agent's errors is nil by default"
    (is (nil? (agent-errors (agent 0)))))

  (testing "an agent's error handler is nil by default"
    (is (nil? (error-handler (agent 0)))))

  (testing "handling errors with an error handler"
    (let [sentinel (atom false)
          agt (agent 0 :error-handler (fn [_ _] (reset! sentinel true)))]
      (send agt seq)
      (await agt)
      (is (true? (deref sentinel)))))

  (testing "allows an error handler to be set after an agent is created"
    (let [agt (agent 0)
          handler-fn (fn [_ _])]
      (set-error-handler! agt handler-fn)
      (is (= handler-fn (error-handler agt)))))

  (testing "await-for a period of time"
    (let [agt (agent 0)]
      (send agt (fn [_] (sleep 100)))
      (is (false? (await-for 1 agt)))))

  (testing "throws runtime exception if agent does not need a restart"
    (let [agt (agent 0)]
      (runtime-exception-is-thrown? #"Agent does not need a restart" (restart-agent agt 1))))

  (testing "allows a failed agent to restart with a new state"
    (let [agt (agent 0)]
      (send agt (fn [_] (throw (new-runtime-exception "welp"))))
      (sleep 100) ; ¯\_(ツ)_/¯
      (restart-agent agt 1)
      (is (= 1 (deref agt)))))

  (testing "meta is nil when not defined"
    (is (nil? (meta (agent "agt")))))

  (testing "creates an agent with meta"
    (let [agt (agent "agt" :meta {:foo :bar})]
      (is (= {:foo :bar} (meta agt)))))

  (testing "meta can be reset on the same object"
    (let [agt (agent "agt" :meta {:foo :bar})
          reset-meta-value (reset-meta! agt {:baz :bang})]
      (is (= {:baz :bang} reset-meta-value))
      (is (= {:baz :bang} (meta agt)))))

  (testing "meta can be altered on the same object"
    (let [agt (agent "agt" :meta {:foo :bar})
          alter-meta-value (alter-meta! agt (fn [_ k v] {k v}) :baz :bang)]
      (is (= {:baz :bang} alter-meta-value))
      (is (= {:baz :bang} (meta agt)))))

  (testing "get-validator will return the current validator function"
    (let [validator-fn #(not= 3 %)
          agt (agent 2 :validator validator-fn)]
      (is (= validator-fn (get-validator agt)))))

  (testing "get-validator is nil if a validator has not been set"
    (is (nil? (get-validator (agent "agt")))))

  (testing "set-validator! will set the current validator function"
    (let [validator-fn #(not= 3 %)
          agt (agent 2)]
      (is (nil? (set-validator! agt validator-fn)))
      (is (= validator-fn (get-validator agt)))))

  )
