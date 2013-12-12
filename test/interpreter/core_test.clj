(ns interpreter.core-test
  (:require [clojure.test :refer :all]
            [interpreter.core :refer :all]))

(alter-var-root (var interpreter.core/*debug*) (constantly false))

(defn eval-in-freshenv
  [exp]
  (let [[env store] (fresh-env)]
    (scheme-eval exp env store halt)))

(deftest constants
  (testing "numbers"
           (is (= 42 (eval-in-freshenv 42))))
  (testing "booleans"
           (is (= false (eval-in-freshenv false))))
  (testing "primitive symbols"
           (is (= + (eval-in-freshenv '+)))))

(deftest quote
  (testing (is (= 'exp (eval-in-freshenv '(quote exp)))))
  (testing (is (= '(foo x y z) (eval-in-freshenv '(quote (foo x y z)))))))

(deftest function-value
  (testing "eval identity function"
    (let [l (eval-in-freshenv '(lambda (x) x))]
      (is (= '(x) (:body l)))
      (is (= '(x) (:params l)))))
  (testing "eval sum function"
    (let [l (eval-in-freshenv '(lambda (x y) (+ x y)))]
      (is (= '((+ x y)) (:body l)))
      (is (= '(x y) (:params l))))))

#_(deftest if-statement
  (testing "true predicate"
           (is (= 'success (eval-in-freshenv '(if (= 42 42) 'success 'fail)))))
  (testing "false predicate"
           (is (= 'success (eval-in-freshenv '(if (= 42 666) 'fail 'success)))))
  #_(testing "take into account side-effect in predicate."
           (is (= 'success (eval-in-freshenv '(let ( (x 42) )
                                                    (if (begin (set! x 12)
                                                               (= x 12))
                                                      'success 'fail)))))))

(deftest application
  (testing "primitive procedures"
           (testing "plus"
                    (is (= 1/2 (eval-in-freshenv '(/ 1 2)))))
           (testing "sum"  ;; TODO !!
                    (is (= 6 (eval-in-freshenv '(+ 1 2 3)))))
           (testing "equals"
                    (is (eval-in-freshenv '(= 42 42)))))
  (testing "identity function"
           (is (= 42 (eval-in-freshenv '((lambda (x) x) 42)))))
  (testing "apply sum function"
           (is (= 42 (eval-in-freshenv '((lambda (x y) (+ x y)) 40 2)))))
  (testing "closure"
           (is (= 42 (eval-in-freshenv '(((lambda (x) (lambda () x)) 42))))))
  #_(testing "Y"
           (is (= 120 (eval-in-freshenv '(((lambda (f1)
                                                       ((lambda (x) (f1 (x x)))
                                                          (lambda (x) (f1 (lambda (y) ((x x) y))))))
                                                 (lambda (f2)
                                                         (lambda (n)
                                                                 (if (= n 0)
                                                                   1
                                                                   (* n (f2 (- n 1))))))) 5))))))

;(deftest define
;  (testing "Numerical value" (= 42 (lookup-variable-value 'universe
;                                                          (second (eval-in-freshenv '(define universe 42)))
;                                                          *the-store*)))
;  (testing "Lambda value" (is (:procedure (lookup-variable-value 'id
;                                                                 (second (eval-in-freshenv '(define id (lambda (x) x))))
;                                                                 *the-store*))))
;  (testing "Define a lambda macro"
;           (is (let [env (second (eval-in-freshenv '(define (id x) x)))]
;                 (= 42 (first (scheme-apply [(lookup-variable-value 'id env *the-store*) env]
;                                            '((42)) env *the-store* halt)))))))
;
;(deftest setBANG
;  (testing "Fails when the variable doesn't exist."
;           (is (try (eval-in-freshenv '(set! universe 42))
;                 (catch IllegalArgumentException e
;                   true))))
;  (testing "Overwrites the value bound to variable."
;           (is (= 666 (let [env (second (eval-in-freshenv '(define universe 42)))]
;                        (eval-assignment 'universe [666 env] *the-store*)
;                        (lookup-variable-value 'universe env *the-store*))))))
;
;(deftest let-macro
;  (testing (is (= 42 (eval-in-freshenv-val '(let ((x 42) (route 66)) x))))))
;
;(deftest begin-exp
;  (testing "Returns last expression" (is (= 3 (eval-in-freshenv-val '(begin 1 2 3)))))
;  (testing "Define within a begin"
;           (is (= 42 (eval-in-freshenv-val '(begin (define x 42) x))))))
;
;(eval-in-freshenv-val '(cond
;                     [(= 2 3) 'wrong!]
;                     [(= 2 2) 'ok]))
;
;(eval-in-freshenv '(cons "asdf" '()))
