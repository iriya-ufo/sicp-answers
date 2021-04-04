;; ex1.46.scm

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
	guess
	((iterative-improve good-enough? improve) (improve guess)))))

(define (square x) (* x x))


;; iterative-improve���Ѥ���sqrt��³��

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.00001))
  (define (improve guess)
    (define (average x y) (/ (+ x y) 2))
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(print (sqrt 2))
(print (sqrt 3))
(newline)


;; iterative-improve���Ѥ���fixed-point��³��

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) 0.00001))
  ((iterative-improve close-enough? f) first-guess))
  
(print (fixed-point cos 1.0))
(print (fixed-point (lambda (y) (+ (sin y) (cos y)))
		    1.0))
