;; ex1.36.scm

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


;; non-average-damping

(print "non-average-damping")

(fixed-point (lambda (x) (/ (log 1000) (log x)))
	     1.01)
(newline)


;; average-damping

(print "average-damping")

(define (average x y) (/ (+ x y) 2))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
	     1.01)
