(define (even? n)
  (= (remainder n 2) 0))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
  (define (multiplication-iter a b product)
    (cond ((= b 0) product)
          ((even? b) (multiplication-iter (double a) (halve b) product))
          (else (multiplication-iter a (- b 1) (+ a product)))))
  (multiplication-iter a b 0))

(* 3 24)
;; => 72
