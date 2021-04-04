;; ex2.05.scm

(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (my-car z)
  (if (even? z)
      (+ 1 (my-car (/ z 2)))
      0))

(define (my-cdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (my-cdr (/ z 3)))
      0))

(define foo (my-cons 5 3))
(print foo)
(print (my-car foo))
(print (my-cdr foo))
(newline)

(define bar (my-cons 0 8))
(print bar)
(print (my-car bar))
(print (my-cdr bar))
