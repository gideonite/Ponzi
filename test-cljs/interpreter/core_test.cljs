(ns interpreter.core-test
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test run-tests testing)])
  (:require [cemerick.cljs.test :as t]
            [interpreter.core]))

(deftest interpret-test
  (testing "interprets numbers"
           (is (= 5 (interpreter.core/interpret 5)))))
