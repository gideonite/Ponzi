(ns interpreter.core-test
  (:require [clojure.test :refer :all]
            [interpreter.core :refer :all]))

(alter-var-root (var interpreter.core/*debug*) (constantly false))

(defn eval-in-freshenv
  ([exp]
   (let [[env store] (fresh-env)]
     (scheme-eval exp env store halt)))
  ([exp k]
   (let [[env store] (fresh-env)]
     (scheme-eval exp env store k))))

(deftest constants
  (testing "Numbers"
           (is (= 42 (eval-in-freshenv 42))))
  (testing "Booleans"
           (is (= false (eval-in-freshenv false))))
  (testing "Primitive symbols"
           (is (= + (eval-in-freshenv '+)))))

(deftest quote
  (testing (is (= 'exp (eval-in-freshenv '(quote exp)))))
  (testing (is (= '(foo x y z) (eval-in-freshenv '(quote (foo x y z)))))))

(deftest function-value
  (testing "Eval identity function"
    (let [l (eval-in-freshenv '(lambda (x) x))]
      (is (= '(x) (:body l)))
      (is (= '(x) (:params l)))))
  (testing "Eval sum function"
    (let [l (eval-in-freshenv '(lambda (x y) (+ x y)))]
      (is (= '((+ x y)) (:body l)))
      (is (= '(x y) (:params l))))))

(deftest if-statement
  (testing "True predicate"
           (is (= 'success (eval-in-freshenv '(if (= 42 42) 'success 'fail)))))
  (testing "False predicate"
           (is (= 'success (eval-in-freshenv '(if (= 42 666) 'fail 'success)))))
  #_(testing "take into account side-effect in predicate."
             ;; TODO
           (is (= 'success (eval-in-freshenv '(let ( (x 42) )
                                                    (if (begin (set! x 12)
                                                               (= x 12))
                                                      'success 'fail)))))))

(deftest application
  (testing "Primitive procedures"
           (testing "plus"
                    (is (= 1/2 (eval-in-freshenv '(/ 1 2)))))
           (testing "sum"
                    (is (= 6 (eval-in-freshenv '(+ 1 2 3)))))
           (testing "equals"
                    (is (eval-in-freshenv '(= 42 42)))))
  (testing "Identity function"
           (is (= 42 (eval-in-freshenv '((lambda (x) x) 42)))))
  (testing "Apply sum function"
           (is (= 42 (eval-in-freshenv '((lambda (x y) (+ x y)) 40 2)))))
  (testing "Closure" (is (= 42 (eval-in-freshenv '(((lambda (x) (lambda () x)) 42)))))
           (is (= 42 (eval-in-freshenv '((((lambda (x) (lambda () (lambda () x))) 42)))))))
  (testing "Multiple sexps in body returns value of last sexp"
           (is (= 84 (eval-in-freshenv '((lambda (x) (* x x) (+ x x)) 42)))))
  (testing "Evaluate a local function"
           (is (= 42 (eval-in-freshenv '((lambda () ((lambda (x) x) 42)))))))
  (testing "Function as arguments"
           (is (= 84 (eval-in-freshenv '((lambda (f)
                                                 (f 42)) (lambda (x) (+ x x)))))))
  (testing "Variable bindings don't bleed out of their proper scope"
           (is (try (eval-in-freshenv '( (lambda () ((lambda (x) x) 42) x)))
                 (catch Exception e ;; TODO Gotta catch 'em all? Make a better exception.
                   true))))
  (testing "Anonymous recursion factorial"
           (is (= 120 (eval-in-freshenv '(((lambda (f)
                                                   (lambda (n)
                                                           (if (= n 0)
                                                             1
                                                             (* n ((f f) (- n 1))))))
                                             (lambda (f)
                                                     (lambda (n)
                                                             (if (= n 0)
                                                               1
                                                               (* n ((f f) (- n 1))))))) 5)))))
  (testing "Y"
           (is (= 120 (eval-in-freshenv '(((lambda (f1)
                                                   ((lambda (x) (f1 (x x)))
                                                      (lambda (x) (f1 (lambda (y) ((x x) y))))))
                                             (lambda (f2)
                                                     (lambda (n)
                                                             (if (= n 0)
                                                               1
                                                               (* n (f2 (- n 1))))))) 5))))))

(deftest define
  (testing "Numerical value"
           (is (eval-in-freshenv '(define universe 42)
                                 (fn [v env store] (= 42 (lookup-variable-value 'universe env store))))))
  (testing "Define within a lambda works"
           (is (nil? (eval-in-freshenv '((lambda () (define x 42))) (fn [v env store] v)))))
  (testing "Define within a lambda does the right thing (shadows whatever follows)."
           (is (= 42 (eval-in-freshenv '((lambda (x) (define x 42) x) 12)))))
  (testing "Infinite loop can be written. don't try this at home."
           (eval-in-freshenv '(define f (lambda () f)) (fn [v env store] ((lookup-variable-value 'f env store) :proc))))
  (testing "But bindings within a lambda don't bleed out."
           (try (eval-in-freshenv '((lambda () ((lambda (x) (define x 42) x) 12) x)))
             (catch Exception e true))))

(deftest setBANG
  (testing "Fails when the variable doesn't exist."
           (is (try (eval-in-freshenv '(set! universe 42))
                 (catch IllegalArgumentException e
                   true))))

  (testing "Overwrites a defined variable."
           (is (= 12 (eval-in-freshenv '((lambda () (define x 42) (set! x 12) x)))))) 

  (testing "Doesn't blead scope."
           (is (= 0 (eval-in-freshenv '((lambda (x) (lambda () (define x 42) (set! x 12)) x) 0))))) )

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
