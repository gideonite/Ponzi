(ns interpreter.core)

(defn ^:export foobar [x]
  (println "hello world"))

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
