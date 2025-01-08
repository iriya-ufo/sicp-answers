;;; a.
(define (pseudoremainder-terms a b)
  (let ((integerizing-factor (expt (coeff (first-term b))
                                   (+ 1
                                      (order (first-term a))
                                      (- (order (first-term b)))))))
    (cadr (div-terms (mul-term-by-all-terms (make-term 0 integerizing-factor) a)
                     b))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (pseudoremainder-terms a b))))

;;; b.
(define (gcd-terms a b)
  (define (old-gcd-terms a b)
    (if (empty-termlist? b)
        a
        (old-gcd-terms b (pseudoremainder-terms a b))))
  (mul-term-by-all-terms
   (make-term 0
              (/ 1 (gcd (map coeff (term-list (old-gcd-terms a b))))))
   (old-gcd-terms a b)))
