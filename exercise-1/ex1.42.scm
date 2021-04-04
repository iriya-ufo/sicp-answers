;; ex1.42.scm

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(print ((compose square inc) 6))
(print ((compose inc square) 6))
