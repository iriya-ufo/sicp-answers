(define (sine x)
  (apply-generic 'sine x))

(define (sine-int n)
  (sin n))

(define (sine-rational r)
  (sin (/ (numer r) (denom r))))

(define (sine-real x)
  (sin x))

(put 'sine 'integer (lambda (x) (sine-int x)))
(put 'sine 'rational (lambda (x) (sine-rational x)))
(put 'sine 'real (lambda (x) (sine-real x)))

(define (cosine x)
  (apply-generic 'cosine x))

(define (cosine-int n)
  (cos n))

(define (cosine-rational r)
  (cos (/ (numer r) (denom r))))

(define (cosine-real x)
  (cos x))

(put 'cosine 'integer (lambda (x) (cosine-int x)))
(put 'cosine 'rational (lambda (x) (cosine-rational x)))
(put 'cosine 'real (lambda (x) (cosine-real x)))
