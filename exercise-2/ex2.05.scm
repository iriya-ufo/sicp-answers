(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (if (even? z)
      (+ 1 (car (/ z 2)))
      0))

(define (cdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (cdr (/ z 3)))
      0))

(define foo (cons 5 3))
(car foo)
;; => 5
(cdr foo)
;; => 3
