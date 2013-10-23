(ns interpreter.core)

(def clojure-apply apply)

;;
;; CREATE ENVIRONMENT
;;

(defn an-empty-environmet
  []
  "returns a fresh empty environment. Internally this is an empty list."
  '())

(defn extend-environment
  [env frame]
  (conj env frame))

(def primitive-procedures
  { (symbol '+) +
    (symbol '-) -
    (symbol '*) *
    (symbol '/) /
    (symbol '=) =
    (symbol '<) <
    (symbol '>) >
    (symbol 'cons) cons
    (symbol 'rest) rest
   })

(defn setup-environment []
  (extend-environment (an-empty-environmet) (atom primitive-procedures)))

;;
;; ENVIRONMENT FUNCTIONS
;;

(declare scheme-eval)

(defn make-frame
  "a frame a atom of a map of variables to values"
  [variables values]
  (atom (zipmap variables values)))

(defn add-binding-to-frame!
  "mutates the frame by either adding a new binding to the variable or
  replacing the old binding with the value provided."
  [frame variable value]
  (swap! frame assoc variable value))

(defn self-evaluating?
  [exp]
  (or (number? exp)))

(defn variable?
  [exp]
  (symbol? exp))

(defn enclosing-environment
  [env]
  (rest env))

(defn lookup-frame
  [variable env]
  (first (filter #(variable @%) env)))

(defn lookup-variable-value
  "Finds the value of the variable in the first frame that contains it.
  Returns nil if no frames contain it."
  [variable env]
  (let [value (variable @(lookup-frame variable env))]
    (if (nil? value)
      (throw (IllegalArgumentException. (str "Unbound symbol: '" variable "'"))))
    value))

(defn definition-variable
  [exp]
  (nth exp 1))

(defn definition-value
  [exp]
  (nth exp 2))

(defn set-variable-value
  "tries to bind the variable to the value in the first frame in the
  environment.  If a frame is found then on-success is run on [frame variable
  value] otherwise, on-fail is run on [environment variable value]"
  [exp env on-success on-fail]
  (let [variable (definition-variable exp)
        value (scheme-eval (definition-value exp) env)
        frame (lookup-frame variable env)]
    (if frame (on-success frame variable value)
      (on-fail env variable value))))

;;
;; PROCEDURE
;;

(defn primitive-procedure?
  [procedure]
  (contains? (into #{} (vals primitive-procedures))
             procedure))

(defn apply-primitive-procedure
  [procedure arguments]
  (clojure-apply procedure arguments))

(defn compound-procedure?
  [procedure]
  (:procedure procedure))

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

;;
;; APPLY
;;
(declare eval-sequence)

(defn scheme-apply
  [procedure arguments]
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure) (eval-sequence
                                          (:body procedure)
                                          (extend-environment (:env procedure)
                                                              (make-frame (:parameters procedure)
                                                                          arguments)))
        :else (throw (IllegalArgumentException.
                       (str "Undefined procedure on arguments: " arguments)))))

;;
;; EVAL
;;

(defn define->lambda
  [exp env]
  (let [[fun & params] `~(definition-variable exp)
        body `~(definition-value exp)]
    `(~'define ~fun (~'lambda ( ~@params) ~body))))

(defn let->lambda
  [exp env]
  (let [[_let bindings body] exp
        vars (map first bindings)
        values (map second bindings)]
    `((~'lambda ~vars ~body) ~@values)))

(defn eval-definition
  "makes the binding to the first frame containing the variable specified in
  the exp, or if no frame contains the variable, adds the binding to the first
  frame in the env"
  [exp env]
  (set-variable-value exp env
                      add-binding-to-frame!
                      #(add-binding-to-frame! (first %1) %2 %3)))

(defn eval-assignment
  "throw an error if there's no frame containing the variable being set in the
  exp"
  [exp env]
  (set-variable-value exp env
                      add-binding-to-frame!
                      #(throw (IllegalArgumentException.
                                (str "SET! unbound symbol '" %2 "'")))))

(defn eval-coll
  [coll env]
  (map #(scheme-eval % env) coll))

(defn tagged-list?
  [exp tag]
  (= tag (first exp)))

(defn assignment?
  [exp]
  (tagged-list? exp 'set!))

(defn begin?
  [exp]
  (tagged-list? exp 'begin))

(defn begin-expressions
  [exp]
  (rest exp))

(defn eval-all
  "Evaluates each expression in exps in the environment env.  Literally maps
  scheme-eval over exps"
  [exps env]
  (map #(scheme-eval % env) exps))

(defn eval-sequence
  "Evaluates each expressions in exps in the environment env.  Returns the
  value of the last expression."
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
  (rest (rest exp)))

(defn operator
  [exp]
  (first exp))

(defn operands
  [exp]
  (rest exp))

(defn quoted?
  [exp]
  (tagged-list? exp 'quote))

(defn text-of-quotation
  [exp]
  (rest exp))

(defn if?
  [exp]
  (tagged-list? exp 'if))

(defn eval-if
  [exp env]
  (if (scheme-eval (nth exp 1) env)
    (scheme-eval (nth exp 2) env)
    (scheme-eval (nth exp 3) env)))

(defn definition?
  [exp]
  (tagged-list? exp 'define))

(defn fun-def?
  [exp]
  (and (definition? exp)
       (list? (definition-variable exp))))

(defn let?
  [exp]
  (tagged-list? exp 'let))

(defn cond?
  [exp]
  (tagged-list? exp 'condy))    ;; prevent clojure macros from expanding `cond`

(defn cond-clauses
  [exp]
  (rest exp))

(defn expand-cond-clauses
  [clauses]
  (if-let [[pred exp] (first clauses)]
    (list 'if pred exp
          (expand-cond-clauses (rest clauses)))))

(defn cond->if
  [exp]
  (expand-cond-clauses (cond-clauses exp)))

(defn scheme-eval
  [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-variable-value exp env)
        (quoted? exp) (text-of-quotation exp)
        (lambda? exp) (make-procedure (parameters exp) (body exp) env)
        (fun-def? exp) (scheme-eval (define->lambda exp env) env)
        (let? exp) (scheme-eval (let->lambda exp env) env)
        (begin? exp) (eval-sequence (begin-expressions exp) env)
        (definition? exp) (eval-definition exp env)
        (assignment? exp) (eval-assignment exp env)
        (if? exp) (eval-if exp env)
        (cond? exp) (scheme-eval (cond->if exp) env)
        (list? exp) (scheme-apply   ;; in SICP this is hidden behind an opaque application abstraction
                      (scheme-eval (operator exp) env)
                      (eval-all (operands exp) env))
        :else (throw (IllegalArgumentException. (str "EVAL error " exp)))))

(defn -main
  [& args]

  (def welcome-msg "welcome!\n\n\n")
  (def prompt "clem>   ")
  (def the-global-env (setup-environment))

  (set! *print-level* 4)

  (clojure.main/repl :init (fn [] (print welcome-msg))
                     :prompt (fn [] (print prompt))
                     :eval (fn [line] (scheme-eval line the-global-env))))
