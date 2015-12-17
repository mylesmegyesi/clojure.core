(ns clojure.lang.platform.afn-test
  (:refer-clojure :only [let])
  (:require [clojure.next           :refer :all]
            [clojure.test           :refer :all]
            [clojure.lang.afn       :refer :all]
            [clojure.lang.protocols :refer :all]))

(deffn PlatformTestFn [called]
  IFn
  (-invoke [this] (do
                    (reset! called true)
                    "testing")))

(deftest platform-afn-runnable-test
  (testing "run will invoke the zero arity function and return nil"
    (let [called (atom false)]
      (is (nil? (.run (PlatformTestFn. called))))
      (is (true? (deref called)))))

  (testing "call will invoke the zero arity function and return the result"
    (let [called (atom false)]
      (is (= "testing" (.call (PlatformTestFn. called))))
      (is (true? (deref called))))))


