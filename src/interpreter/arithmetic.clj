(ns interpreter.arithmetic)

(def lookup
  {'+ +
   '- -
   '/ /
   '* *})

(def primitive-procs (set (vals lookup)))

(defn halt [val]
  (println "HALT!" val)
  val)

(defn evaluate
  [exp k]
  (cond
    (number? exp) (k exp)
    ((complement nil?) (primitive-procs exp)) (k exp)
    ((complement nil?) (lookup exp)) (k (lookup exp))
    :else (let [[f left right] exp]
            (evaluate left (fn [l]    ;; push left
                             (evaluate right (fn [r] ;; push right
                                               (evaluate f (fn [fun] ;; push f
                                                             (evaluate (fun l r) (fn [v] (k v)))))))))))) ;; push k

(comment
  (evaluate '1 halt)
  (evaluate '+ halt)
  (evaluate '(+ 1 1) halt)
  (evaluate '(- 1 1) halt)
  (evaluate '(/ 1 2) halt)
  (evaluate '(* 2 1) halt)

  (evaluate '(+ (+ 1 2) (+ 3 (+ 4 5))) halt)
  )

