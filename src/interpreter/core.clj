(ns interpreter.core)

(def clojure-apply apply)

;;
;; ENVIRONMENT CREATION
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
   })

(defn setup-environment []
  (extend-environment (an-empty-environmet) (atom primitive-procedures)))

;;
;; ENVIRONMENT FUNCTIONS
;;

(defn make-frame
  "a frame a atom of a map of variables to values"
  [variables values]
  (atom (zipmap variables values)))

(defn add-binding-frame!
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
    (if (not value)
      (throw (Exception. (str "Unbound symbol: '" variable "'"))))
    value))

(defn set-variable-value
  "Replaces the first frame containing the variable with a new frame in which
  the variable's current value is replaced with the value provided.  Returns a
  lazy-seq *of the new environment*."
  [variable value env]
  (lazy-seq
    (if-let [[frame & frames] env]
      (if (variable frame)
        (cons (swap! assoc variable value) frames)
        (cons frame (set-variable-value variable value frames)))
      (throw (Exception. (str "Unbound variable: SET! " variable))))))

;; TODO: custom Exception?

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

(defn arguments
  [exp]
  (rest exp))

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

(declare eval-sequence)

(defn scheme-apply
  [procedure arguments]
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure) (eval-sequence
                                          (:body procedure)
                                          (extend-environment (:env procedure)
                                                              (make-frame (:parameters procedure)
                                                                          arguments)))
        :else (throw (Exception. (str "Undefined procedure on arguments: " arguments)))))

(defn assignment-variable
  [exp]
  (second exp))

(defn assignment-value
  [exp]
  (nth exp 2))

(declare scheme-eval)

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
  (nth exp 1))

(defn definition-value
  [exp]
  (nth exp 2))

(defn eval-definition
  "makes the binding to the first frame containing the variable specified in
  the exp, or if no frame contains the variable, adds the binding to the first
  frame in the env"
  [exp env]
  (let [variable (definition-variable exp)
        value (definition-value exp)
        lookup (lookup-frame variable env)
        frame (if lookup lookup (first env))
        evaluated (scheme-eval value env)]
    (add-binding-frame! frame variable evaluated)
    ))

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

(defn scheme-eval
  [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-variable-value exp env)
        (quoted? exp) (text-of-quotation exp)
        (begin? exp) (eval-sequence (begin-expressions exp) env)
        (lambda? exp) (make-procedure (parameters exp) (body exp) env)
        (definition? exp) (eval-definition exp env)
        (if? exp) (eval-if exp env)
        (list? exp) (scheme-apply   ;; in SICP this is hidden behind an opaque application abstraction
                      (scheme-eval (operator exp) env)
                      (eval-all (operands exp) env))
        :else (throw (Exception. (str "EVAL error " exp)))))

(defn -main
  [& args]

  (def welcome-msg "welcome!\n\n\n")
  (def prompt "clem>   ")
  (def the-global-env (setup-environment))

  (clojure.main/repl :init (fn [] (print welcome-msg))
                     :prompt (fn [] (print prompt))
                     :eval (fn [line] (scheme-eval line the-global-env))))
