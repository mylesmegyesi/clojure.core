(ns clojure.lang.chunk-buffer-test
  (:refer-clojure :only [let])
  (:require [clojure.test                         :refer :all]
            [clojure.next                         :refer :all]
            [clojure.lang.chunk-buffer            :refer :all]
            [clojure.support.exception-assertions :refer [out-of-bounds-exception-is-thrown?]]))

(deftest chunk-buffer-test
  (testing "add increases the count"
    (let [cbuff (chunk-buffer 3)]
      (-add cbuff :foo)
      (is (= 1 (count cbuff)))
      (-add cbuff :bar)
      (is (= 2 (count cbuff)))
      (-add cbuff :baz)
      (is (= 3 (count cbuff)))))

  (testing "an out of bounds exception is thrown is an element is added after max capacity"
    (let [cbuff (chunk-buffer 1)]
      (-add cbuff 0)
      (out-of-bounds-exception-is-thrown? #"" (-add cbuff 1))))

  (testing "an chunk is returned when asking for the chunk"
    (let [cbuff (chunk-buffer 2)]
      (-add cbuff 0)
      (-add cbuff 1)
      (let [achunk (-chunk cbuff)]
        (is (= 2 (count achunk)))
        (is (= 0 (nth achunk 0)))
        (is (= 1 (nth achunk 1)))))))

