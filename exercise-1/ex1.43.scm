;; ex1.43.scm

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

(define (cube x) (* x x x))

(print ((repeated square 2) 5))
(print ((repeated square 1) 5))
(print ((repeated cube 2) 2))
