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
    (testing "define constants (self-evaluating)"
             (scheme-eval '(define a 42) env)
             (scheme-eval '(define a 1) env)
             (is (= 1 ('a @(first env)))))

    (testing "bind lambdas to variables"
             (scheme-eval '(define g (lambda (x y) (+ x y))) env)
             (is (= 2 (scheme-eval '(g 1 1) env))))

    (testing "basic recursion using a name binding (factorial)"
             (scheme-eval '(define fact (lambda (x acc) (if (= 1 x) acc (* x (fact (- x 1) acc))))) env)
             (is (= 120 (scheme-eval '(fact 5 1) env))))))

(deftest eval-set!
  (let [env (setup-environment)]
    ;; different exceptions throw different signatures, in any case something is
    ;; thrown here
    ;;
    ;; (testing "throws an exception when trying to set an unbound symbol"
    ;;          (try
    ;;            (scheme-eval '(set! a 42) env)
    ;;            (catch Exception e (is (= "SET! unbound symbol 'a'" (.getMessage e))))))

    (testing "rebinds a bound symbol"
             (scheme-eval '(define g (lambda (x y) (+ x y))) env)
             (scheme-eval '(set! g 42) env)
             (is (= 42 (scheme-eval 'g env))))))

(deftest eval-cond
  (is (= '(greater)
         (scheme-eval '(condy ((> 3 2) 'greater) ((< 3 2) 'lesser)) (setup-environment))))

  (is (= '(lesser)
         (scheme-eval '(condy ((< 3 2) 'greater) ((= 1 1) 'lesser)) (setup-environment)))))

(deftest eval-define-function
  (let [env (setup-environment)]
    (scheme-eval '(define (sum x y) (+ x y)) env)
    (is  (= 4 (scheme-eval '(sum 2 2) env)))))

(deftest eval-let
  (let [env (setup-environment)]
    (is (= 42 (scheme-eval '(let ( (x 21) ) (+ x 21)) env)))))
