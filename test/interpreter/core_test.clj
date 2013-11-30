(ns interpreter.core-test
  (:require [clojure.test :refer :all]
            [interpreter.core :refer :all]))

(defn eval-in-freshenv
  [exp]
  (let [[env store] (fresh-env)]
    (scheme-eval exp env store)))

(defn eval-in-freshenv-val
  [exp]
  (first (eval-in-freshenv exp)))

(deftest constants
  (testing "numbers"
           (is (= 42 (eval-in-freshenv-val 42))))
  (testing "booleans"
           (is (= false (eval-in-freshenv-val false))))
  (testing "primitive symbols"
           (is (= + (eval-in-freshenv-val '+)))))

(deftest quote
  (testing (is (= '(exp) (eval-in-freshenv-val '(quote exp))))))

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
           (is (= 2 (eval-in-freshenv '(+ 1 1))))))
