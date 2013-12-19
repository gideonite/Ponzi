(ns interpreter.core
  (:require [clojure.core.match :refer [match]]))

(def clojure-apply apply)

(def ^:dynamic *debug* false)

(defmacro log [& xs] `(when *debug* (println ~@xs)))

(defn new-env [] '())

(defn fresh-store [] {})

(defn getval [[value env]] value)

(defn extend-environment
  "env frame -> new env."
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
         bindings bindings
         store store]
    (if (seq bindings)
      (let [[variable value] (first bindings)
            addr (gensym)]
        (recur (assoc frame variable addr)
               (rest bindings)
               (assoc store addr value)))
      [frame store])))

(defn fresh-env []
  "-> [env store]"
  (let [[frame store] (make-frame primitive-procedures (fresh-store))]
  [(extend-environment (new-env) frame) store]))

(declare scheme-eval)

(defn lookup-variable-value
  "Symbol env store -> value (or nil).

  Finds the value of the variable in the first frame that contains it.
  Returns nil if no frames contain it."
  [variable env store]
  (if-let [frame (first (filter #(% variable) env))]
    (let [addr (frame variable)
          value (store addr)]
      value)
    (throw (Exception.
             (str "Undefined variable '" variable "'")))))

(def primitive-procedure?
  (memoize (fn [procedure]
             (contains? (into #{} (vals primitive-procedures))
                        procedure))))

(defn compound-procedure?
  [procedure]
  (:procedure procedure))

(defn make-procedure
  "Returns the datum representing procedure.
  Has a field :procedure which is set to true"
  [parameters body env]
  {:proc true
   :params parameters
   :body body
   :env env})

(declare eval-sequence)

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
  "
  Creates a new binding in the first frame containing the variable, or creates
  a new binding in the base environment (first frame), and then does this:

  (k nil new-environment new-store)
  "
  [variable value env store k]
  (log "eval-def:" variable ", first frame:" (first (filter #(% variable) env)))
  (if-let [frame (first (filter #(% variable) env))]
    (let [addr (frame variable)]
      (k nil env (assoc store addr value)))
    (let [[frame store] (make-frame [[variable value]] store)
          new-frame (merge frame (first env))]
      (k nil (cons new-frame (rest env)) store))))

(defn eval-assignment
  [variable value env store k]
  (log "assignment!" variable value)
  (if-let [frame (first (filter #(% variable) env))]
    (let [addr (frame variable)]
      (k nil env (assoc store addr value)))
    (throw (IllegalArgumentException.
             (str "SET! unbound symbol '" variable "'")))))

(defn cond->if
  [pairs]
  (if-let [pair (first pairs)]
    `(if ~(first pair) ~(second pair)
       ~(cond->if (rest pairs)))
    nil))

(defn halt
  "A slight variation on the identity function which prints 'HALT!' to stdout."
  [val env store]
  (println "HALT!" val (count env) (count store))
  val)

;; TODO: optimize this!
(defn primitive?
  [sym]
  (contains? (set (keys primitive-procedures)) sym))

(defn eval-all
  "Evaluates all the expressions. Takes a continuation that will do something
  to a list of expressions."
  [exps env store k]
  (fn []
    (match [exps]
           [([] :seq)] (k '() env store)
           [([x & xs] :seq)] (scheme-eval x env store
                                          (fn [v env store]
                                            (eval-all xs env store
                                                      (fn [vs env store]
                                                        (k (cons v vs) env store))))))))

(defn eval-sequence
  "Evaluates a list of expressions, and then does this:
  (k last-value env store)

  Think of it as a begin expression."
  [exps env store k]
  (fn []
    (match [exps]
           [([x] :seq)] (scheme-eval x env store (fn [v env store] (k v env store)))
           [([x & xs] :seq)] (scheme-eval x env store
                                          (fn [v env store]
                                            (eval-sequence xs env store (fn [last-value env store]
                                                                          (k last-value env store))))))))

(defn apply-proc [{:keys [params body env] :as f} vs store k]
  "Unwraps a procedure, creates bindings between the parameters and the
  arguments (thus adding to the store) and then evaluates the body as a begin
  expression."
  (let [[bindings store] (make-frame (partition 2 (interleave params vs)) store)
        env (extend-environment env bindings)]
    (eval-sequence body env store k)))

(defn tramp-scheme-eval
  "exp env store k -> value."
  [exp env store k]
  (log "eval" "#frames:" (count env) "   " "#bindings:" (count store))
  (match [exp]
         [(_ :guard #(or (number? %) (string? %) (false? %) (true? %)))] (fn [] (k exp env store))
         [(_ :guard symbol?)] (fn [] (k (lookup-variable-value exp env store) env store))
         [(['quote & e] :seq)] (fn [] (k (first e) env store))
         [(['lambda & e] :seq)] (fn [] (let [parameters (first e) body (rest e)]
                                         (k (make-procedure parameters body env) env store)))

         [(['if pred x else] :seq)] (fn [] (tramp-scheme-eval pred env store (fn [pred env store]
                                                                               (if pred
                                                                                 (tramp-scheme-eval x env store (fn [x env store] (k x env store)))
                                                                                 (tramp-scheme-eval else env store (fn [else env store] (k else env store)))))))

         [(['define (sym :guard (complement seq?)) v] :seq)] (fn [] (tramp-scheme-eval v env store (fn [v env store]
                                                                                                     (eval-definition sym v env store k)))) 

         [(['set! sym v] :seq)] (fn [] (tramp-scheme-eval v env store (fn [v env store]
                                                                        (eval-assignment sym v env store k))))

         [(['define (_ :guard seq?) & r] :seq)] (fn [] (tramp-scheme-eval (definefun->lambda exp) env store k))
         [(['let ( _ :guard seq?) & r] :seq)] (fn [] (tramp-scheme-eval (let->lambda exp) env store k))

         [(['begin & e] :seq)] (fn [] (eval-sequence e env store k))
         [(['cond & e] :seq)] (fn [] (tramp-scheme-eval (cond->if e) env store k))

         [([( f :guard primitive?) & exps] :seq)] (fn [] (tramp-scheme-eval f env store
                                                                            (fn [f env store] (eval-all exps env store
                                                                                                        (fn [vs env store]
                                                                                                          (k (apply f vs) env store))))))

         :else (fn [] (let [[f & exps] exp]
                        (tramp-scheme-eval f env store
                                           (fn [f f-env store] (eval-all exps f-env store
                                                                         (fn [vs args-env store] (apply-proc f vs store
                                                                                                             (fn [v final-env store]
                                                                                                               (k v f-env store)))))))))))

(defn my-trampoline
  ([f]
   (let [ret (f)]
     (match [ret]
            [(_ :guard primitive?)] ret
            [(_ :guard #(and (fn? %) (comp not primitive? %)))] (recur ret)
            :else ret)))
  ([f & args]
   (my-trampoline #(apply f args))))

(defn scheme-eval
  [exp env store k]
  (my-trampoline tramp-scheme-eval exp env store k))

(defn repl
  [v env store]
  (println v)
  (print "=>  ")
  (flush)
  (let [exp (try (read-string (read-line))
              (catch RuntimeException e ""))]
    (scheme-eval exp env store
                (fn [v env store] (repl v env store)))))

(defn -main
  [& args]
  (let [[env store] (fresh-env)]
    (if (seq? args)
      (doseq [filename args]
        (doseq [form (read-string (str \( (slurp filename) \)))]
          (scheme-eval form env store (fn [v env store] v))))
      (repl "Welcome to Ponzi" env store))))
