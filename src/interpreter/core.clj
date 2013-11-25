(ns interpreter.core
  (:require [clojure.core.match :refer [match]]))

(def clojure-apply apply)

;;
;; CREATE ENVIRONMENT
;;

(defn new-env [] '())
(defn fresh-store [] {})

(defn extend-environment
  [env [frame store]]
  [(conj env frame) store])

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
    (symbol 'print) println
   })

(defn make-frame
  "[Sequable [variable value] pairs] store -> [frame store].
  Takes a lists of bindings and returns a new frame and a new store."
  [bindings store]
  (loop [frame {}
         store store
         bindings (map (fn [  [sym proc]]
                              [sym (gensym) proc])
                       (seq bindings))]
    (if (seq bindings)
      (let [[variable gsym value] (first bindings)]
        (recur (assoc frame variable gsym)
               (assoc store gsym value)
               (rest bindings)))
      [frame store])))

(defn fresh-env []
  "-> [env store]"
  (extend-environment (new-env)
                      (make-frame primitive-procedures (fresh-store))))

;;
;; ENVIRONMENT FUNCTIONS
;;

(declare scheme-eval)

(defn add-binding-to-frame!
  "mutates the frame by either adding a new binding to the variable or
  replacing the old binding with the value provided."
  [frame variable value]
  (swap! frame assoc variable value))

(defn enclosing-environment
  [env]
  (rest env))

(defn lookup-frame
  [variable env]
  (first (filter #(variable @%) env)))

(defn lookup-variable-value
  "symbol [env store] -> value (or nil).
  Finds the value of the variable in the first frame that contains it.
  Returns nil if no frames contain it."
  [variable [env store]]
  ;(println env)
  (when-let [frame (first (filter #(% variable) env))]
    (let [sym (frame variable)
          value (store sym)]
      value)))





;; TODO left off with this. No longer using lookup-frame (should I get rid of
;; it). Continue going through the various cases and figuring out how to
;; support them. Presumably, now that lookup works, the next part of the job is
;; to figure out how to add to the environment. What are the cases here?
;; -Lambdas -define -set! Right?

(defn definition-variable
  [exp]
  (nth exp 1))

(defn definition-value
  [exp]
  (nth exp 2))

#_(defn set-variable-value
  "Tries to bind the variable to the value in the first frame in the
  environment. If a frame is found then on-success is run on [frame variable
  value] otherwise, on-fail is run on [environment variable value]."
  [exp [env store] on-success on-fail]
  (let [variable (definition-variable exp)
        value (scheme-eval (definition-value exp) [env store])
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
  "Returns the datum representing procedure.
  Has a field :procedure which is set to true"
  [parameters body env]
  {:procedure true
   :parameters parameters
   :body body
   :env env})

;;
;; APPLY
;;
(declare eval-sequence)

(defn scheme-apply
  [procedure arguments store]
  (cond (primitive-procedure? procedure) (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure) (eval-sequence
                                          (:body procedure)
                                          (extend-environment (:env procedure)
                                                              (make-frame (map vector
                                                                               (:parameters procedure) arguments) store)))
        :else (throw (IllegalArgumentException.
                       (str "Undefined procedure on arguments: " arguments)))))

;;
;; EVAL
;;

(defn definefun->lambda
  "(define (fun p q) <body>) -> (define fun (lambda p q) <body>)"
  [exp]
  (let [[fun & params] `~(definition-variable exp)
        body `~(definition-value exp)]
    `(~'define ~fun (~'lambda ( ~@params) ~body))))

(defn let->lambda
  "let form -> closed over lambda form"
  [exp]
  (let [[_let bindings body] exp
        vars (map first bindings)
        values (map second bindings)]
    `((~'lambda ~vars ~body) ~@values)))

#_(defn eval-definition
  "makes the binding to the first frame containing the variable specified in
  the exp, or if no frame contains the variable, adds the binding to the first
  frame in the env"
  [exp [env store]]
  (set-variable-value exp [env store]
                      add-binding-to-frame!
                      #(add-binding-to-frame! (first %1) %2 %3)))

#_(defn eval-assignment
  "throw an error if there's no frame containing the variable being set in the
  exp"
  [exp env]
  (set-variable-value exp env
                      add-binding-to-frame!
                      #(throw (IllegalArgumentException.
                                (str "SET! unbound symbol '" %2 "'")))))

(defn begin-expressions
  [exp]
  (rest exp))

(defn eval-all
  "Evaluates each expression in exps in the environment env.  Literally maps
  scheme-eval over exps"
  [exps [env store]]
  (map #(scheme-eval % [env store]) exps))

(defn eval-sequence
  "Evaluates each expressions in exps in the environment env.  Returns the
  value of the last expression."
  [exps [env store]]
  (last (eval-all exps [env store])))

(defn eval-if
  [exp env store]
  (if (scheme-eval (nth exp 1) [env store])
    (scheme-eval (nth exp 2) [env store])
    (scheme-eval (nth exp 3) [env store])))

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
  [exp [env store]]
  (match [exp]
    [(_ :guard number?)] exp
    [(:or (_ :guard false?) (_ :guard true?))] exp
    [(_ :guard symbol?)] (lookup-variable-value exp [env store])
    [(['quote & e] :seq)] e
    [(['lambda & e] :seq)] (let [parameters (first e) body (list (second e))]
                             (make-procedure parameters body env))
    ;[(['define (sym :guard (complement seq?)) v] :seq)] (eval-definition exp env)
    [(['define (_ :guard seq?) & r] :seq)] (scheme-eval (definefun->lambda exp) [env store])
    [(['let ( _ :guard seq?) & r] :seq)] (scheme-eval (let->lambda exp) [env store])
    [(['begin & e] :seq)] (eval-sequence (begin-expressions exp) env) ;; TODO : remove begin-expressions
    ;[(['set! & e] :seq)] (eval-assignment exp env)
    [(['if & e] :seq)] (eval-if exp [env store])
    [(['cond & e] :seq)] (scheme-eval (cond->if exp) [env store])
    :else (scheme-apply (scheme-eval  (first exp) [env store])
                        (eval-all     (rest exp)  [env store])
                        store)))
(comment
  (scheme-eval 42 (fresh-env))
  (scheme-eval false (fresh-env))
  (scheme-eval '+ (fresh-env))
  (scheme-eval '(quote exp) (fresh-env))
  (scheme-eval '(lambda (x) x) (fresh-env))
  (scheme-eval '(lambda (x y) (+ x y)) (fresh-env))
  (scheme-eval '(+ 1 1) (fresh-env))
  (scheme-eval '((lambda (x) x) 42) (fresh-env))
  (scheme-eval '((lambda (x y) (+ x y)) 1 1) (fresh-env))
  (scheme-eval '(a b c) (fresh-env))  ;; TODO: improve this error

  ;; (scheme-eval '(define universe 42) (fresh-env))
  ;; (scheme-eval '(define id (lambda (x) x)) (fresh-env))
  ;; (scheme-eval '(define (id x) x) (fresh-env))
  ;; (scheme-eval '(let ( (x 42) ) x) (fresh-env))
  ;; (scheme-eval '(begin (+ 42 42) 42) (fresh-env))
  ;; (scheme-eval '(begin (define x 42) (set! x (/ 42 2)) x) (fresh-env))
  ;; (scheme-eval '(if (= 42 42) 0 1) (fresh-env))
  ;; (scheme-eval '(cond ( (= 42 42) 42) ( (= 12 12) 12)) (fresh-env))
  )

;; TODO string literals.
;; TODO give cond the power of else

(defn -main
  [& args]

  (def welcome-msg "welcome!\n\n\n")
  (def prompt "hmm> ")
  (def the-global-env (fresh-env))

  (set! *print-level* 4)

  (if (seq? args)
    (doseq [filename args]
      (doseq [form (read-string (str \( (slurp filename) \)))]
        (scheme-eval form the-global-env)))
    #_(clojure.main/repl :init (fn [] (print welcome-msg))
                       :prompt (fn [] (print prompt))
                       :eval (fn [line] (scheme-eval line the-global-env)))))

;;
;; CESK Machine
;; Matt Might
;; Dan Friedman
;; Andrew Appel --- Princeton
;;
;; TODO:  Internal define?
;;        Write a macro-expander
;; letrec let-loop
;;
;; CPS your interpreter --- makes it easier to implement trampolines.
