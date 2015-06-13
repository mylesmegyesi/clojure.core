(ns clojure.lang.agent
  (:refer-clojure :only [apply declare defmacro defn defn- defprotocol deftype let loop pos? vec >])
  (:require [clojure.next                   :refer :all]
            [clojure.lang.atomic-ref        :refer :all]
            [clojure.lang.persistent-queue  :as    queue]
            [clojure.lang.persistent-vector :refer [EMPTY-VECTOR]]
            [clojure.lang.protocols         :refer [IAgent IDeref IMeta IReference IValidatable IWatchable
                                                    -action-queue -cons -error-handler -set-error-handler -error-mode -enqueue -notify-watches -peek -reset-meta! -set-state -restart]]
            [clojure.lang.runnable          :refer :all]
            [clojure.lang.thread            :refer [create-fixed-thread-pool-executor
                                                    create-cached-thread-pool-executor
                                                    local-state get-local-state set-local-state]]
            [clojure.lang.exceptions        :refer [new-runtime-exception platform-try]]))

; This allows us to avoid implementing LockingTransaction right
; now. We'll need to revisit at some point.
(def ^{:private true} locking-transaction
  (let [field (.getDeclaredField  clojure.lang.LockingTransaction "transaction")]
    (.setAccessible field true)
    (.get (.get field clojure.lang.LockingTransaction))))

(def ^{:private true} nested (local-state))

(def ^{:private true} pooled-counter (new-atomic-long 0))
(def pooled-executor (create-fixed-thread-pool-executor pooled-counter))
(def ^{:private true} solo-counter (new-atomic-long 0))
(def solo-executor (create-cached-thread-pool-executor solo-counter))

(defprotocol ^{:private true} IActionQueue
  (-stack [this])
  (-error [this]))

(deftype ^{:private true} ActionQueue
  [-stack -error]

  IActionQueue
  (-stack [this] -stack)
  (-error [this] -error))

(defn new-action-queue [stack error]
  (ActionQueue. stack error))

(def EMPTY-ACTION-QUEUE (new-action-queue queue/EMPTY-QUEUE nil))

(defprotocol ^{:private true} IAction
  (-agent [this])
  (-args [this])
  (-fn [this])
  (-exec [this])
  (-execute [this]))

(defn action-release-pending-sends []
  (let [sends (get-local-state nested)]
    (if (nil? sends)
      0
      (loop [s sends]
        (if (empty? s)
          (do
            (set-local-state nested EMPTY-VECTOR)
            (count sends))
          (let [act (first s)]
            (-enqueue (-agent act) act)))))))

(defn agent-set-error-handler [agnt error-fn]
  (-set-error-handler agnt error-fn))

(defn agent-get-error-handler [agnt]
  (-error-handler agnt))

(defn- do-action-run2 [action agnt error]
  (let [nxt (loop [popped false
                   nxt nil]
              (if popped
                nxt
                (let [prior (ref-get (-action-queue agnt))
                      next-nxt (new-action-queue (pop (-stack prior)) error)
                      next-popped (ref-compare-and-set! (-action-queue agnt) prior next-nxt)]
                  (recur next-popped next-nxt))))]
    (if (and (nil? error) (pos? (count (-stack nxt))))
      (-execute (-peek (-stack nxt))))))

(defn- do-action-run [action error]
  (let [agnt (-agent action)]
    (if (nil? error)
      (do
        (action-release-pending-sends)
        (do-action-run2 action agnt nil))
      (do
        (set-local-state nested nil)
        (let [err-handler (-error-handler agnt)]
          (if err-handler
            (platform-try
              (err-handler agnt error)
              (platform-catch clojure.lang.exceptions/throwable e)))
          (if (= :continue (-error-mode agnt))
            (do-action-run2 action agnt nil)
            (do-action-run2 action agnt error)))))))

(defmacro ^{:private true} apply-to [f args]
  `(apply ~f (vec ~args)))

(defrunnable Action [-agnt -ar -f -ex]
  IAction
  (-agent [this] -agnt)
  (-args [this] -ar)
  (-fn [this] -f)
  (-exec [this] -ex)
  (-execute [this]
    (platform-try
      (invoke-execute -ex this)
      (platform-catch clojure.lang.exceptions/throwable error
        (if (-error-handler -agnt)
          (platform-try
            ((-error-handler -agnt) error)
            (platform-catch clojure.lang.exceptions/throwable _ nil))))))
  (-run [this]
    (try
      (set-local-state nested EMPTY-VECTOR)
      (platform-try
        (let [old-val (deref -agnt)
              new-val (apply-to -f (clojure.core/cons old-val -ar))]
          (-set-state -agnt new-val)
          (-notify-watches -agnt old-val new-val)
          (do-action-run this nil))
        (platform-catch clojure.lang.exceptions/throwable e
          (do-action-run this e)))
      (finally
        (set-local-state nested nil)))))

(defn new-action [-agent -args -fn -exec]
  (Action. -agent -args -fn -exec))

(defn agent-get-error [agnt]
  (-error (ref-get (-action-queue agnt))))

(defn agent-restart [agnt new-state options]
  (-restart agnt new-state options))

(declare new-action)

(deftype Agent [^:volatile-mutable -state
                ^:volatile-mutable -error-handler
                ^:unsynchronized-mutable -meta
                ^:volatile-mutable -validator
                ^:volatile-mutable -watches
                ^:volatile-mutalbe -error-mode
                -action-queue]

  IAgent
  (-action-queue [this] -action-queue)

  (-error-handler [this] -error-handler)

  (-set-error-handler [this f]
    (set! -error-handler f))

  (-error-mode [this] -error-mode)

  (-enqueue [this action]
    (let [p (loop [queued false
                   prior nil]
              (if queued
                prior
                (let [prior (ref-get -action-queue)
                      queued (ref-compare-and-set! -action-queue prior (new-action-queue (-cons (-stack prior) action) (-error prior)))]
                      (recur queued prior))))]
      (if (and (zero? (count (-stack p))) (nil? (-error p)))
        (-execute action))))

  (-dispatch [this f args executor]
    (let [error (agent-get-error this)]
      (if error
        (throw (new-runtime-exception "Agent is failed, needs restart" error))
        (let [action (new-action this args f executor)]
          (if locking-transaction
            (.enqueue locking-transaction action)
            (if (get-local-state nested)
              (set-local-state nested (clojure.core/cons (get-local-state nested) action))
              (-enqueue this action)))
          this))))

  (-set-state [this new-state]
    (set! -state new-state))

  (-restart [this new-state options]
    (if (agent-get-error this)
      (let [clear-actions (get options :clear-actions)]
        (-set-state this new-state)
        (if clear-actions
          (ref-set! -action-queue EMPTY-ACTION-QUEUE)
          (let [q (loop [restarted false
                         prior nil]
                    (if restarted
                      (-stack prior)
                      (let [p (ref-get -action-queue)]
                        (recur
                          (ref-compare-and-set! -action-queue p (new-action-queue (-stack p) nil))
                          p))))]
            (if (> (count q) 0)
              (-execute (-peek q)))))
        new-state)
      (throw (new-runtime-exception "Agent does not need a restart"))))

  IDeref
  (-deref [this] -state)

  IMeta
  (-meta [this] -meta)

  IReference
  (-reset-meta! [this new-meta]
    (set! -meta new-meta)
    new-meta)

  (-alter-meta! [this f args]
    (let [meta-args (clojure.core/cons -meta args)
          new-meta (apply f meta-args)]
      (-reset-meta! this new-meta)))

  IValidatable
  (-get-validator [this] -validator)

  (-set-validator! [this f] (set! -validator f) nil)

  IWatchable
  (-notify-watches [this old-val new-val] nil)

  )

(declare EMPTY-ACTION-QUEUE)

(defn new-agent [-state -error-handler -meta -validator -watches -error-mode]
  (Agent. -state -error-handler -meta -validator -watches -error-mode
    (new-atomic-ref EMPTY-ACTION-QUEUE)))

