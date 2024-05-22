(define (raise x)
  (apply-generic 'raise x))

(define (raise-int n)
  (make-rational n 1))

(define (raise-rational r)
  (attach-tag 'scheme-number (/ (numer r) (denom r))))

(define (raise-real x)
  (make-from-real-imag x 0))

(put 'raise 'integer (lambda (x) (raise-int x)))
(put 'raise 'rational (lambda (x) (raise-rational x)))
(put 'raise 'real (lambda (x) (raise-real x)))
