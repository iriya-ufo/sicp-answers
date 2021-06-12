(define (fast-expt b n)
  (define (fast-expt-iter a b counter)
    (if (= counter 0)
        a
        (if (even? counter)
            (fast-expt-iter a (* b b) (/ counter 2))
            (fast-expt-iter (* a b) b (- counter 1)))))
  (fast-expt-iter 1 b n))

(define (even? n)
  (= (remainder n 2) 0))

(fast-expt 2 8)   ;; => 256
(fast-expt 2 10)  ;; => 1024
(fast-expt 0.1 3) ;; => 0.0010000000000000002
