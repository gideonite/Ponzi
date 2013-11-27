;; fact
(lambda (f)
        (lambda (n)
                (if (= n 0)
                  1
                  (* n (f (- n 1))))))

;; Y combinator
(lambda (f)
         ((lambda (x) (f (x x)))
          (lambda (x) (f (lambda (y) ((x x) y))))))


;; factorial (recursive! thank you y combinator)
(((lambda (f1)
          ((lambda (x) (f1 (x x)))
           (lambda (x) (f1 (lambda (y) ((x x) y))))))

  (lambda (f2)
          (lambda (n)
                  (if (= n 0)
                    1
                    (* n (f2 (- n 1))))))) 5)
