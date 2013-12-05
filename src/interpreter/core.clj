(ns interpreter.core
  (:require [clojure.core.match :refer [match]]))

(def clojure-apply apply)

(def ^:dynamic *the-store* (atom {}))

(def ^:dynamic *debug* false)

(defmacro log [& xs] `(when *debug* (println ~@xs)))

(defn new-env [] '())
(defn fresh-store [] (reset! *the-store* {}) *the-store*)

(defn getval [[value env]] value)

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
    (symbol 'print) println
    'nil nil
   })

(defn make-frame
  "[& [variable value]] store -> [frame store].
  Takes a lists of bindings and returns a new frame and a new store."
  [bindings store]
  (loop [frame {}
         bindings bindings]
    (if (seq bindings)
      (let [[variable value] (first bindings)
            addr (gensym)]
        (swap! store assoc addr value)
        (recur (assoc frame variable addr)
               (rest bindings)))
      frame)))

(defn fresh-env []
  "-> [env store]"
  [(extend-environment (new-env)
                       (make-frame primitive-procedures (fresh-store)))
   *the-store*])

(declare scheme-eval)

(defn add-binding-to-frame!
  "mutates the frame by either adding a new binding to the variable or
  replacing the old binding with the value provided."
  [frame variable value]
  (swap! frame assoc variable value))

(defn lookup-variable-value
  "Symbol env store -> value (or nil).

  Finds the value of the variable in the first frame that contains it.
  Returns nil if no frames contain it."
  [variable env store]
  (when-let [frame (first (filter #(% variable) env))]
    (log "found-frame!" frame)
    (let [addr (frame variable)
          value (@store addr)]
      value)))

(def primitive-procedure?
  (memoize (fn [procedure]
             (contains? (into #{} (vals primitive-procedures))
                        procedure))))

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

(declare eval-sequence)

(defn scheme-apply
  [procedure arguments env store]
  (let [proc (getval procedure)
        args (map getval arguments)]

    (log "apply, proc:"  proc)
    (log "primtive-procedure?" (primitive-procedure? proc))
    (log "compound-procedure?" (compound-procedure? proc))

    (cond (primitive-procedure? proc)
            [(apply-primitive-procedure proc args) env]
          (compound-procedure? proc)
            (eval-sequence (:body proc)
                           (extend-environment (:env proc)
                                               (make-frame (map vector (:parameters proc) args) store))
                           store)
            :else (throw (IllegalArgumentException.
                          (str "Undefined procedure: <" procedure "> on arguments: " (seq arguments)))))))

(defn definefun->lambda
  "(define (fun p q) <body>) -> (define fun (lambda p q) <body>)"
  [exp]
  (let [[fun & params] `~(nth exp 1)
        body `~(nth exp 2)]
    `(~'define ~fun (~'lambda ( ~@params) ~body))))

(defn let->lambda
  "let form -> closed over lambda form"
  [exp]
  (let [[_let bindings body] exp
        vars (map first bindings)
        values (map second bindings)]
    `((~'lambda ~vars ~body) ~@values)))

(defn eval-definition
  "Variable value env store -> [nil env].

  Binds the variable to the value in the first frame containing the variable.
  If no frame contains the variable, adds the binding to the first frame.
  "
  [variable [value env] store]
  (log "eval-def:" variable ", first frame:" (first (filter #(% variable) env)))
  (if-let [frame (first (filter #(% variable) env))]
    (let [addr (frame variable)]
      (swap! store assoc addr value)
      [nil env])
    (let [frame (make-frame [[variable value]] store)
          new-frame (merge frame (first env))]
      [nil (cons new-frame (rest env))])))

(defn eval-assignment
  [variable [value env] store]
  (log "assignment!" variable value)
  (if-let [frame (first (filter #(% variable) env))]
    (let [addr (frame variable)]
      (swap! store assoc addr value)
      [value env])
    (throw (IllegalArgumentException.
             (str "SET! unbound symbol '" variable "'")))))

(defn begin-expressions
  [exp]
  (rest exp))

(defn eval-all
  "Evaluates each expression in exps in the environment env. Literally maps
  scheme-eval over exps."
  [exps env store]
  (map #(scheme-eval % env store) exps))

(defn eval-sequence
  "Evaluates each expressions in exps in the environment env. Returns the
  value of the last expression."
  [exps env store]
  (loop [exps exps
         env env]
    (let [[value env] (scheme-eval (first exps) env store)]
      (if (seq (rest exps))
        (recur (rest exps) env)
        [value env]))))

(defn eval-if
  [exp env store]
  (log "eval-if" (scheme-eval (nth exp 1) env store))
  (let [[v env] (scheme-eval (nth exp 1) env store)]
    (or (and v (scheme-eval (nth exp 2) env store))
        (scheme-eval (nth exp 3) env store))))

(defn cond-clauses
  [exp]
  (rest exp))

(defn expand-cond-clauses
  [clauses]
  (if-let [[pred exp] (first clauses)]
    (list 'if pred exp
          (expand-cond-clauses (rest clauses)))))

#_(defn cond->if
  [pairs]
  (loop [pairs pairs
         if-exp 'n]
    (if (seq pairs)
      (let [[pred exp] (first pairs)]
      (recur (rest pairs)
             (concat (list if-exp) `(if ~pred ~exp))))
      if-exp)))

(defn cond->if
  [pairs]
  (if-let [pair (first pairs)]
    `(if ~(first pair) ~(second pair)
       ~(cond->if (rest pairs)))
    nil))

(defn scheme-eval
  "exp env store -> [value env]."
  [exp env store]
  (log "eval" exp)
  (match [exp]
         [(_ :guard number?)] [exp env]
         [(:or (_ :guard false?) (_ :guard true?))] [exp env]
         [(_ :guard symbol?)] [(lookup-variable-value exp env store) env]
         [(['quote & e] :seq)] [(first e) env]
         [(['lambda & e] :seq)] (let [parameters (first e) body (rest e)]
                                  [(make-procedure parameters body env) env])
         [(['if & e] :seq)] (eval-if exp env store)
         [(['define (sym :guard (complement seq?)) v] :seq)] (eval-definition sym (scheme-eval v env store) store)
         [(['set! sym v] :seq)] (eval-assignment sym (scheme-eval v env store) store)
         [(['define (_ :guard seq?) & r] :seq)] (scheme-eval (definefun->lambda exp) env store)
         [(['let ( _ :guard seq?) & r] :seq)] (scheme-eval (let->lambda exp) env store)
         [(['begin & e] :seq)] (eval-sequence e env store)
         [(['cond & e] :seq)] (scheme-eval (cond->if e) env store)
         :else (scheme-apply (scheme-eval  (first exp) env store)
                             (eval-all     (rest exp) env store)
                             env
                             store)))

(defn eval-loop
  [exps [env store]]
  (loop [exps exps
         env env]
    (recur (rest exps)
           (second (scheme-eval
                     (first exps) [env store])))))

#_(defn -main
  [& args]

  (def welcome-msg "welcome!\n\n\n")
  (def prompt "hmm> ")
  (def the-global-env (fresh-env))

  (set! *print-level* 4)

  (if (seq? args)
    (doseq [filename args]
      (doseq [form (read-string (str \( (slurp filename) \)))]
        (scheme-eval form the-global-env)))
    (clojure.main/repl :init (fn [] (print welcome-msg))
                       :prompt (fn [] (print prompt))
                       :eval (fn [line] (scheme-eval line the-global-env)))))
