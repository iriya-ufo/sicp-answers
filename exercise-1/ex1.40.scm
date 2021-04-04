;; ex1.40.scm

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (cond ((close-enough? guess next)
	     (print next))
	    (else (print guess)
		  (try next)))))
  (try first-guess))


(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;; x = 0,2,-2 を解とする3次方程式
;; y = x(x-2)(x+2) = x^3 - 4x

(newtons-method (cubic 0 -4 0) 0.3)
(newtons-method (cubic 0 -4 0) 5.0)
(newtons-method (cubic 0 -4 0) -5.0)

;; x = -1,3,5 を解とする3次方程式
;; y = (x+1)(x-3)(x-5) = x^3 - 7x^2 + 7x + 15

(newtons-method (cubic -7 7 15) 4.3)
(newtons-method (cubic -7 7 15) 0.8)
