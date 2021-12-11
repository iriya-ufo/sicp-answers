(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n 1)
         (+ (d 1) (cont-frac (lambda (i) (n (+ i 1)))
                             (lambda (i) (d (+ i 1)))
                             (- k 1))))))

(define (e-2 k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i)
               (if (= (remainder i 3) 2)
                   (* 2 (+ 1 (quotient i 3)))
                   1.0))
             k))

(define (e k) (+ 2 (e-2 k)))

(e 10)
;; => 2.7182817182817183

(e 11)
;; => 2.7182818352059925

(e 12)
;; => 2.7182818229439496
