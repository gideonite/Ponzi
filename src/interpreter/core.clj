(ns interpreter.core)

(def clojure-apply apply)

(def primitive-procedures
  {:plus  +
   :minus -
   :multiply *
   :divide /})

;; interpret : eval and apply

(defn self-evaluating?
  [exp]
  (cond
    (number? exp) true))

(defn variable?
  [exp]
  (symbol? exp))

(defn lookup-variable-value
  [exp env]
  ;; TODO
  )

(defn primitive-procedure?
  [procedure]
  (contains? primitive-procedures procedure))

(defn apply-primitive-procedure
  [procedure arguments]
  (clojure-apply procedure arguments))

(defn compound-procedure?
  [procedure]
 false)

(defn scheme-apply
  [procedure arguments]
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure) "foobar"))

(defn operator
  [exp]
  (first exp))

(defn operands
  [exp]
  (rest exp))

(declare scheme-eval)
(defn list-of-values
  [operands env]
  (map #(scheme-eval % env) operands))

(defn application? [exp] exp)

(defn scheme-eval
  [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp)     (lookup-variable-value exp env)
        (application? exp)  (scheme-apply (scheme-eval (operator exp) env)
                                          (list-of-values (operands exp) env))))

(scheme-eval 1 nil)

(scheme-eval `(+ 1 1) nil)

;; The difference between a symbol and a variable:
;;    * symbol:     datatype, could be used for anything
;;    * variable:   interpreter concept/data type, the interpreter delegates
;;                  based on the type of data it is getting (whether it is a
;;                  variable, a number, a lambda, etc.)
