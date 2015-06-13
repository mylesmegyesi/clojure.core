(ns clojure.lang.meta-test
  (:refer-clojure :only [apply cons deftype let select-keys])
  (:require [clojure.test           :refer :all]
            [clojure.lang.protocols :refer [IMeta IObj IReference -reset-meta!]]
            [clojure.next           :refer :all :exclude [cons]]))

(deftype TestMeta [^:unsynchronized-mutable -meta]
  IMeta
  (-meta [this] -meta)

  IObj
  (-with-meta [this new-meta]
    (TestMeta. new-meta))

  IReference
  (-reset-meta! [this new-meta]
    (set! -meta new-meta)
    new-meta)

  (-alter-meta! [this f args]
    (let [meta-args (cons -meta args)
          new-meta (apply f meta-args)]
      (-reset-meta! this new-meta))))

(deftest meta-test
  (testing "meta returns nil for objects that do not implement IMeta"
    (is (nil? (meta 1))))

  (testing "meta returns the meta of an object"
    (let [mta {:so :meta}
          obj (TestMeta. mta)]
      (is (= mta (meta obj)))))

  (testing "with-meta returns a new instance with the specified meta"
    (let [obj1 (TestMeta. {})
          obj2 (with-meta obj1 {:so :meta})]
      (is (not= obj1 obj2))
      (is (= {:so :meta} (meta obj2)))))

  (testing "reset-meta! will reset the meta on the object"
    (let [obj (TestMeta. {})]
      (reset-meta! obj {:so :meta})
      (is (= {:so :meta} (meta obj)))))

  (testing "alter-meta! will reset the meta on the object by applying the given function with the given arguments"
    (let [obj (TestMeta. {:a 1 :b 2 :c 3})]
      (alter-meta! obj select-keys '(:b))
      (is (= {:b 2} (meta obj)))))

  (testing "vary-meta will apply a function and given arguments to with-meta"
    (let [obj1 (TestMeta. {:a 1 :b 2 :c 3})
          obj2 (vary-meta obj1 select-keys '(:b))]
      (is (= {:a 1 :b 2 :c 3} (meta obj1)))
      (is (= {:b 2} (meta obj2))))))

