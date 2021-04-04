;; ex1.38.scm

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate-rec combiner null-value term (next a) next b))))

(define (cont-frac n d k)
  (define (combiner x cf)
    (/ (n x) (+ (d x) cf)))
  (define (term i) i)
  (define (next i) (+ i 1))
  (accumulate-rec combiner (/ (n k) (d k)) term 1 next (- k 1)))

(define (e-2 k)
  (cont-frac (lambda (i) 1.0)
	     (lambda (i)
	       (if (= (remainder i 3) 2)
		   (* 2 (+ 1 (quotient i 3)))
		   1.0))
	     k))

(define (e k) (+ 2 (e-2 k)))

(print (e 9))
(print (e 10))
(print (e 11))
(print (e 12))
