(define even?
        (lambda (n)
                (if (eq? 0 n)
                  #t
                  (odd? (- n 1)))))

(define odd?
        (lambda (n)
                (if (eq? 1 n)
                  #t
                  (even? (- n 1)))))

(even? 0)
(odd? 1)
