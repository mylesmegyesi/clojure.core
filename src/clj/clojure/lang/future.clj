(ns clojure.lang.future
  (:refer-clojure :only [defn let])
  (:require [clojure.next                   :refer :all]
            [clojure.lang.future-submission :refer [deffuture get-result is-done? submit-future]]
            [clojure.lang.protocols         :refer [IDeref IBlockingDeref IPending
                                                    -deref -blocking-deref -is-realized?]]))

(deffuture Future [future-submission]
  IDeref
  (-deref [this] (get-result future-submission))

  IBlockingDeref
  (-blocking-deref [this timeout-ms timeout-val]
    (get-result future-submission timeout-ms timeout-val))

  IPending
  (-is-realized? [this] (is-done? future-submission)))

(defn new-future [f]
  (let [future-submission (submit-future f)]
    (Future. future-submission)))
