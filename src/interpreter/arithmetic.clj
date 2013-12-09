(ns interpreter.arithmetic)

(def primitive-procedures
  {'+ +
   '- -
   '/ /
   '* *})

(defn evaluate [exp]
  (if (seq? exp)
    (let [[proc & body] exp]
      ((primitive-procedures proc)
         (evaluate (first body)) (evaluate (second body))))
    exp))

(evaluate '(+ 1 1))
