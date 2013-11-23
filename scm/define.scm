(define x 42)

(print x)

(define square
  (lambda (x)
    (* x x)))

(print (square x))

(define undefined
  (lambda () undefined))
