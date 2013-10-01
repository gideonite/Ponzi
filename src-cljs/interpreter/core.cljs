(ns interpreter.core)

(def operators {:plus +
                :minus -
                :multiply *
                :divide /
                :number identity
                })

(defn ^:export interpret [expression]
  (if (= :number (first expression))
    ((operators (first expression))
       (second expression))
    (let [[operator l r] expression]
            ((operators operator)
               (interpret l)
               (interpret r)))))

(defn ^:export concrete->token [chunk]
  (if (number? chunk)
    chunk
    (chunk {"+" :plus
            "-" :minus
            "*" :multiply
            "/" :divide})))

(defn ^:export lexer [concrete]
  "concrete string -> list of tokens"
  (clojure.string/split concrete))
