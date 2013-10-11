(ns interpreter.core)

(def clojure-apply apply)

;; a frame is a Clojure map of variables to values
;;
;; an environment is a list of frames

(defn make-frame
  [variables values]
  (zipmap variables values))

(defn extend-environment
  [env frame]
  (cons frame env))

(defn self-evaluating?
  [exp]
  (cond
    (number? exp) true))

(defn variable?
  [exp]
  (symbol? exp))

symbol

(defn enclosing-environment
  [env]
  (rest env))

(defn lookup-variable-value
  [variable env]
  (variable (first (filter variable env))))

(def primitive-procedures)

(defn primitive-procedure?
  [procedure]
  (contains? (into #{} (vals primitive-procedures))
             procedure))

(defn apply-primitive-procedure
  [procedure arguments]
  (clojure-apply procedure arguments))

(defn compound-procedure?
  [procedure]
 false)

(defn arguments
  [exp]
  (rest exp))

(defn scheme-apply
  [procedure arguments]
  (cond (primitive-procedure? procedure)
        (apply-primitive-procedure procedure arguments)))

(defn operator
  [exp]
  (first exp))

(defn operands
  [exp]
  (rest exp))

(declare scheme-eval)

(defn eval-coll
  [coll env]
  (map #(scheme-eval % env) coll))

(defn application? [exp] (seq? exp))
(defn scheme-eval
  [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp)     (lookup-variable-value exp env)
        (application? exp)  (scheme-apply (scheme-eval (operator exp) env)
                                          (eval-coll (operands exp) env))))

(def primitive-procedures
  { (symbol '+) +
    (symbol '-) -
    (symbol '*) *
    (symbol '/) / })

(def the-empty-environmet '())

(defn setup-environment []
  (extend-environment the-empty-environmet primitive-procedures))

(def ^:dynamic *the-global-env* (setup-environment))
