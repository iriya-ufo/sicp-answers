(define (average x y)
  (/ (+ x y) 2))

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve)
         (improve guess)))))

;;; sqrt with iterative-improve
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 3)
;; => 1.7321428571428572

(sqrt 5)
;; => 2.2360688956433634

;;; fixed-point with iterative-improve
(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) 0.0001))
  ((iterative-improve close-enough? f) first-guess))

(fixed-point cos 1.0)
;; => 0.7391301765296711

(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)
;; => 1.2587758014705526
