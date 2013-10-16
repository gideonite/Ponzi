(ns interpreter.core)

(def clojure-apply apply)

;; interact with environment

(defn make-frame
  [variables values]
  (zipmap values variables))

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

;; environment setup

(def primitive-procedures
  { (symbol '+) +
    (symbol '-) -
    (symbol '*) *
    (symbol '/) / })

(def the-empty-environmet '())

(defn setup-environment []
  (extend-environment the-empty-environmet primitive-procedures))

(def ^:dynamic *the-global-env* (setup-environment))

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
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)))

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
  (tagged-list? exp 'quote))

(defn text-of-quotation
  [exp]
  (rest exp))

(defn set-variable-value
  "Replaces the first frame containing the variable with a new frame in which
  the variable's current value is replaced with the value provided.  Returns a
  lazy-seq *of the new environment*."
  [variable value env]
  (lazy-seq
    (if-let [[frame & frames] env]
      (if (variable frame)
        (cons (assoc frame variable value) frames)
        (cons frame (set-variable-value variable value frames)))
      (throw (Exception. (str "Unbound variable: SET! " variable))))))

;; TODO: custom Exception?

(comment
  (set-variable-value :a 12 '({:a 40} {:b 12})))

(defn assignment?
  [exp]
  (tagged-list? exp 'set!))

(defn assignment-variable
  [exp]
  (second exp))

(defn assignment-value
  [exp]
  (nth exp 2))

(declare scheme-eval)

;(defn defintion?
;  [exp]
;  (tagged-list? exp 'define))

(defn define-variable
  [variable value env]
  (lazy-seq
    (if-let [[frame & frames] env]
      (if (variable frame)
        (cons (assoc frame variable value) frames)
        (cons frame (set-variable-value variable value frames)))
      (cons (assoc (first env) variable value) (rest env)))))

(defn definition-variable
  [exp]
  (second exp))

(defn definition-value
  [exp]
  (second exp))

(defn eval-definition
  [exp env]
  (define-variable (definition-variable exp) (definition-value exp) env))

(defn eval-assignment
  [exp env]
  ;; N.B. the eval call is now on the outside whereas in SICP it is on the
  ;; inside.  It is so because the new environment has to be threaded through
  ;; all the calls to eval Is this something insightful?
  (scheme-eval (assignment-value exp)
               (set-variable-value (assignment-variable exp)
                                   (assignment-value exp))))

(defn eval-coll
  [coll env]
  (map #(scheme-eval % env) coll))

(defn begin?
  [exp]
  (tagged-list? exp 'begin))

(defn begin-expressions
  [exp]
  (rest exp))

(defn eval-all
  [exps env]
  (map #(scheme-eval % env) exps))

(defn eval-sequence
  [exps env]
  (last (eval-all exps env)))

(defn lambda?
  [exp]
  (tagged-list? exp 'lambda))

(defn parameters
 [exp]
  (second exp))

(defn body
 [exp]
  (nth exp 2))

(defn make-procedure
  "returns the datum representing procedure.
  Has a field :procedure which is set to true"
  [parameters body env]
  ;; N.B. lambdas evaluate to procedures and that's it!  (Crack open a REPL and
  ;; evaluate `(lambda(x,y) (+ x y))`, it evaluates to some sort of procedure
  ;; object.  When an enclosing S-expression drops through to the `list?` case
  ;; then `apply` kicks in, asks whether it is a procedure call and does the
  ;; appropriate thing).
  {:procedure true
   :parameters parameters
   :body body
   :env env})

(defn scheme-eval
  [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-variable-value exp env)
        (quoted? exp) (text-of-quotation exp)
        (begin? exp) (eval-sequence (begin-expressions exp) env)
        (lambda? exp) (make-procedure (parameters exp) (body exp) env)
        ;(definition? exp) (eval-definition exp env)
        ;(assignment? exp) (eval-assignment exp env)
        (list? exp) (scheme-apply (scheme-eval (operator exp) env) (eval-all (operands exp) env))))

(scheme-eval '(+ 1 1) *the-global-env*)
