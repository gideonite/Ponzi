(ns interpreter.core-test
  (:require [clojure.test :refer :all]
            [interpreter.core :refer :all]))

(defn eval-in-freshenv
  [exp]
  (let [[env store] (fresh-env)]
    (scheme-eval exp env store)))

(defn eval-in-freshenv-val
  [exp]
  (getval (eval-in-freshenv exp)))

(deftest constants
  (testing "numbers"
           (is (= 42 (eval-in-freshenv-val 42))))
  (testing "booleans"
           (is (= false (eval-in-freshenv-val false))))
  (testing "primitive symbols"
           (is (= + (eval-in-freshenv-val '+)))))

(deftest quote
  (testing (is (= 'exp (eval-in-freshenv-val '(quote exp)))))
  (testing (is (= '(foo x y z) (eval-in-freshenv-val '(quote (foo x y z)))))))

(deftest lambda
  (testing "eval identity function"
    (let [l (eval-in-freshenv-val '(lambda (x) x))]
      (is (= '(x) (:body l)))
      (is (= '(x) (:parameters l)))))
  (testing "eval sum function"
    (let [l (eval-in-freshenv-val '(lambda (x y) (+ x y)))]
      (is (= '((+ x y)) (:body l)))
      (is (= '(x y) (:parameters l))))))

(deftest application
  (testing "primitive procedure"
           (is (= 2 (eval-in-freshenv-val '(+ 1 1)))))
  (testing "identity function"
           (is (= 42 (eval-in-freshenv-val '((lambda (x) x) 42)))))
  (testing "apply sum function"
           (is (= 42 (eval-in-freshenv-val '((lambda (x y) (+ x y)) 40 2)))))
  (testing "closure"
           (is (= 42 (eval-in-freshenv-val '(((lambda (x) (lambda () x)) 42))))))
  (testing "Y"
           (is (= 120 (eval-in-freshenv-val '(((lambda (f1)
                                                       ((lambda (x) (f1 (x x)))
                                                          (lambda (x) (f1 (lambda (y) ((x x) y))))))
                                                 (lambda (f2)
                                                         (lambda (n)
                                                                 (if (= n 0)
                                                                   1
                                                                   (* n (f2 (- n 1))))))) 5))))))

(deftest if-statement
  (testing "true predicate"
           (is (= 'success (eval-in-freshenv-val '(if (= 42 42) 'success 'fail)))))
  (testing "false predicate"
           (is (= 'success (eval-in-freshenv-val '(if (= 42 666) 'fail 'success))))))

(deftest define
  (testing "Numerical value" (= 42 (lookup-variable-value 'universe
                                                          (second (eval-in-freshenv '(define universe 42)))
                                                          *the-store*)))
  (testing "Lambda value" (is (:procedure (lookup-variable-value 'id
                                                                 (second (eval-in-freshenv '(define id (lambda (x) x))))
                                                                 *the-store*))))

  (testing "Define a lambda macro"
           (is (let [env (second (eval-in-freshenv '(define (id x) x)))]
                 (= 42 (first (scheme-apply [(lookup-variable-value 'id env *the-store*) env]
                                            '((42)) env *the-store*)))))))

(deftest setBANG
  (testing "Fails when the variable doesn't exist."
           (is (try (eval-in-freshenv '(set! universe 42))
                 (catch IllegalArgumentException e
                   true))))
  (testing "Overwrites the value bound to variable."
           (is (= 666 (let [env (second (eval-in-freshenv '(define universe 42)))]
                        (eval-assignment 'universe [666 env] *the-store*)
                        (lookup-variable-value 'universe env *the-store*))))))
