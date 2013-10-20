(ns interpreter.core-test
  (:require [clojure.test :refer :all]
            [interpreter.core :refer :all]))

(deftest make-frame-test
  (testing "contructs frames"
           (is (= @(make-frame [:x (symbol '+)] [1 +])
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

(deftest text-of-quotation-test
  (is (= '(1 2 3)
         (text-of-quotation '(:quote 1 2 3)))))

;; eval driven tests

(deftest eval-self-evaluating
  (is (= 1 (scheme-eval 1 (setup-environment)))))

(deftest eval-primitive-procedure
  (is (= 2 (scheme-eval '(+ 1 1) (setup-environment)))))

(deftest eval-define
  (let [env (setup-environment)]
    (scheme-eval '(define a 42) env)
    (= 1 (count env))
    (= 42 ('a @(first env)))

    (scheme-eval '(define a 1) env)
    (= 1 ('a @(first env)))))
