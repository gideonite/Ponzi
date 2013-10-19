(ns interpreter.y-combinator
  (:require [interpreter.core :refer :all]))

;; Some notes for working out the y combinator for the factorial function.  It
;; is a forary into what is possible to do using only lambdas and no variable
;; bindings (and thus no good old fashioned recursion).

(comment
  (define (factorial n)
    (if (= n 0)
      1
      (* n (factorial (- n 1)))))

  (define factorial
    (lambda (n)
      (if (= n 0)
        1
        (* n (factorial (- n 1))))))

  (define almost-factorial
    (lambda (f)
      (lambda (n)
        (if (= n 0)
          1
          (* n (f (- n 1)))))))

  (define factorial (almost-factorial factorial))


  (define factorial0 (almost-factorial identity))
  (lambda (n)
         (if (= n 0)
            1
            (* n (identity (- n 1)))))

  (define factorial1 (almost-factorial factorial0)
    (lambda (n)
            (if (= n 0)
              1
              (* n (factorial0 (- n 1))))))


  (define factorial*
    (almost-factorial
      ;; ...
      (almost-factorial
        (almost-factorial
          (almost-factorial identity)))))


  ;; we're done factorial* works!


  ;; fixed points

  (Y f) = (f (Y f))

  (define (Y f) (f Y f))

  ;; or

  (define Y
    (lambda (f)
            (f (Y f))))

  ;; which works!


  ;; ...but it's not a combinator

  '(lambda (self)
          (lambda (n)
                  (if (= n 0))
                  1
                  (* n (self self) (- n 1))))

  '(lambda (self)                 ;; partial-factorial function
           (lambda (f)
                   (lambda (n)    ;; almost-factorial function
                           (if (= n 0))
                           1
                           (* n (f (- n 1))))
                   (self self)))

  ;; factorial
  '((lambda (self)
            (lambda (f)
                    (lambda (n)
                            (if (= n 0))
                            1
                            (* n (f (- n 1))))
                    (self self)))
    (lambda (self)
            (lambda (f)
                    (lambda (n)
                            (if (= n 0))
                            1
                            (* n (f (- n 1))))
                    (self self))))

  ;; in summary, it looks like this

  '((lambda (x) (f (x x)))        ;; where f is almost-factorial
     (lambda (x) (f (x x))))

  ;; but this requires lazy evaluation, how can we do it without lazy evaluation?

  '(lambda (f)
          ((lambda (x) (f (x x)))
             (lambda (x) (f (lambda (y) ((x x) y))))))

  '(lambda (f)
          ((lambda (x) (f (x x)))
             (lambda (x) (f (lambda (y) ((x x) y))))))


  (scheme-eval
    '(((lambda (f)
              ((lambda (x) (f (x x)))
                  (lambda (x) (f (lambda (y) ((x x) y))))))
        (lambda (me)
                (lambda (n)
                        (if (= n 0)
                          1
                          (* n (me (- n 1)))
                          )))) 10) the-global-env)

;; factorial
(((lambda (f)
          ((lambda (x) (f (x x)))
             (lambda (x) (f (lambda (y) ((x x) y))))))
    (lambda (me)
            (lambda (n)
                    (if (= n 0)
                      1
                      (* n (me (- n 1)))
                      ))))
    10)
)
