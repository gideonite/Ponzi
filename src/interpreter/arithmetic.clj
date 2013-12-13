(ns interpreter.arithmetic
  (:require [clojure.core.match :refer [match]]))

(defn halt [val]
  (println "HALT!" val)
  val)

(defn eval-stack
  [exp]
  (match [exp]
         [(_ :guard number?)] exp
         [(_ :guard #{'+ '- '/ '*})] (eval exp)
         [([ op left right] :seq)] ((eval-stack op) (eval-stack left) (eval-stack right))
         :else "ERROR!"))

(comment
  (eval-stack 1)
  (eval-stack '+)
  (eval-stack '(+ 1 1))
  (eval-stack '(+ 1 (+ 2 3)))
  (eval-stack '(+ 1 (/ 2 3))))

(defn eval-cont
  [exp k]
  (println exp)
  (match [exp]
         [(_ :guard number?)] (k exp)
         [(_ :guard #{'+ '- '/ '*})] (k (eval exp))
         [([op left right] :seq)] (eval-cont op (fn [op]
                                                   (eval-cont left (fn [left]
                                                                     (eval-cont right
                                                                                (fn [right] (k (op left right))))))))
         :else "ERROR!"))

(comment
  (eval-cont '1 halt)
  (eval-cont '+ halt)
  (eval-cont '(+ 1 1) halt)
  (eval-cont '(- 1 1) halt)
  (eval-cont '(/ 1 2) halt)
  (eval-cont '(* 2 1) halt)
  (eval-cont '(+ (+ 1 2) (+ 3 (+ 4 5))) halt))
