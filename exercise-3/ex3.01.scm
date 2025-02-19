(define (make-accumulator sum)
  (lambda (num)
    (begin (set! sum (+ num sum))
           sum)))

(define A (make-accumulator 5))

(A 10)
(A 10)
