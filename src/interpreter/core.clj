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

(defn enclosing-environment
  [env]
  (rest env))

(defn lookup-variable-value
  "Finds the value of the variable in the first frame that contains it.
  Returns nil if no frames contain it."
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

(defn tagged-list?
  [exp tag]
  (= tag (first exp)))

(defn quoted?
  [exp]
  (tagged-list? exp :quote))

(defn text-of-quotation
  [exp]
  (rest exp))

(defn replace-first
  "Replaces the first element of the coll that returns true on the pred
  function with replacement."
  [pred coll replacement]
  (loop [lhs '()
         rhs coll]
    (if (pred (first rhs))
      (flatten (list lhs replacement (rest rhs)))
      (recur (cons (first rhs) lhs) (rest rhs)))))

(defn set-variable-value!
  [variable value env]
  (replace-first variable env value))

(defn assignment?
  [exp]
  (tagged-list? exp :set!))

(defn assignment-variable
  [exp])

(defn assignment-value
  [exp])

(declare scheme-eval)

(defn eval-assignment
  [exp env]
  (set-variable-value! (assignment-variable exp)
                       (scheme-eval (assignment-value exp))
                       env))

(defn eval-coll
  [coll env]
  (map #(scheme-eval % env) coll))

(defn application? [exp] (seq? exp))

(defn scheme-eval
  [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-variable-value exp env)
        (quoted? exp) (text-of-quotation exp)
        (assignment? exp) (eval-assignment exp env)
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
