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
           (is (primitive-procedure? +))))

(deftest tagged-list?-test
  (is (= true (tagged-list? '(:quote '(1 2 3)) :quote))))

(deftest quoted?-test
  (testing ""
           (is (= true (quoted? '(quote (1 2 3)))))
           (is (= false (quoted? '(1 2 3 4))))))

(deftest lookup-variable-value-test
  (testing "environment of size 1"
           (is (= :foobar (lookup-variable-value :a '({:a :foobar})))))
  (testing "environment of size > 1"
           (is (= :foobar (lookup-variable-value :a '({:b 12} {:a :foobar})))))
  (testing "the empty environment"
           (is (= nil (lookup-variable-value :a '())))))

;(deftest set-variable-value-test
;  (testing "variable exists"
;           (is (= '( {:a 2} {:b 2 :c 3})
;                  (set-variable-value :a 2 '({:a 1} {:b 2 :c 3})))))
;  (testing "variable does not exist"
;           (is (= nil (set-variable-value :x 2 '({:a 1} {:b 2 :c 3}))))))

(deftest text-of-quotation-test
  (is (= '(1 2 3)
         (text-of-quotation '(:quote 1 2 3)))))

(deftest eval-begin
  (is (= 2 (scheme-eval '(begin 1 (+ 1 1)) (setup-environment)))))

(deftest eval-lambda
  (is (= {:procedure true
          :parameters '(x y)
          :body '(+ 40 2)
          :env (setup-environment) }
         (scheme-eval '(lambda (x, y) (+ 40 2)) (setup-environment)))))

(deftest eval-test
  (testing "evaluates numbers"
           (is (= (scheme-eval 1 (setup-environment)) 1)))
  (testing "evaluates primitive procedures"
           (is (= (scheme-eval '+ (setup-environment)) +))))
