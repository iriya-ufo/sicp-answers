(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

(define (cube x) (* x x x))

((repeated square 2) 5)
((repeated square 1) 5)
((repeated cube 2) 2)
