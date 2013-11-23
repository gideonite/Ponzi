(define even?
        (lambda (n)
                (if (eq? 0 n)
                  true
                  (odd? (- n 1)))))

(define odd?
        (lambda (n)
                (if (eq? 0 n)
                  false
                  (even? (- n 1)))))

(even? 0)
(odd? 1)
