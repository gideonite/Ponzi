(define (even? n) (if (= n 0) true  (odd?  (- n 1))))
(define (odd? n)  (if (= n 0) false (even? (- n 1))))

(print (even? 42))
(print (even? 41))

(print (odd? 42))
(print (odd? 41))

(print (odd? 0))
(print (even? 0))
