(ns interpreter.core-test
  (:require [clojure.test :refer :all]
            [interpreter.core :refer :all]))

(deftest make-frame-test
  (testing "contructs frames"
           (is (= (make-frame [:x (symbol '+)] [1 +])
                  {:x 1 '+ +}))))

(deftest extend-environment-test
  (testing "extends an empty environment"
           (is (= (extend-environment '() {:x 1})
                  '({:x 1}))))
  (testing "extends a nonempty environment"
           (is (= (extend-environment '({:x 1}) {:y 2})
                  '({:y 2} {:x 1})))))

(deftest primitive-procedure?-test
  (testing ""
           (is (primitive-procedure? '+))))

(deftest eval-test
  (testing "evaluates numbers"
           (is (= (scheme-eval 1 (setup-environment)) 1)))
  (testing "evaluates primitive procedures"
           (is (= (scheme-eval '+ (setup-environment)) +))))
