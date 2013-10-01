(ns interpreter.core-test
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testing)])
  (:require [cemerick.cljs.test :as t]
            [interpreter.core]))

(deftest interpret-test
  (testing "interprets numbers"
           (is (= 5 (interpreter.core/interpret `(:number 5)))))
  (testing "interprets plus"
           (is (= 0 (interpreter.core/interpret
                       `(:plus (:number 2) (:number -2))))))
  (testing "interprets minus"
           (is (= 0 (interpreter.core/interpret
                       `(:plus (:number 2) (:number 2))))))
  (testing "interprets times"
           (is (= 4 (interpreter.core/interpret
                       `(:plus (:number 2) (:number 2))))))
  (testing "interprets times zero"
           (is (= 0 (interpreter.core/interpret
                       `(:plus (:number 0) (:number 2))))))
  (testing "interprets divides"
           (is (= 1 (interpreter.core/interpret
                       `(:plus (:number 2) (:number 2)))))))
