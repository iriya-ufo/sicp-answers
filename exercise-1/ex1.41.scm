(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))
(define (square x) (* x x))

((double inc) 2)
((double square) 2)
(((double (double double)) inc) 5)      ; => 21
(((d1 (d2 d3)) inc) 5)
(((d2 d3) ((d2 d3) inc)) 5)
(((d2 d3) (d3 (d3 inc))) 5)
((d3 (d3 (d3 (d3 inc)))) 5)

(((double (double double)) inc) 5)
(((double double) ((double double) inc)) 5)
(((double double) (double (double inc))) 5)
((double (double (double (double inc)))) 5)
