;; ex1.37.scm

;; a. k-term finite continued fraction

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

(print (cont-frac (lambda (i) 1.0)
		  (lambda (i) 1.0)
		  9))

(print (cont-frac (lambda (i) 1.0)
		  (lambda (i) 1.0)
		  10))

(print (cont-frac (lambda (i) 1.0)
		  (lambda (i) 1.0)
		  11))

(print (cont-frac (lambda (i) 1.0)
		  (lambda (i) 1.0)
		  12))
(newline)

;; kの値が11以上で4桁の精度の近似を得る


;; b. 反復的プロセス

(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate-iter combiner (combiner null-value (term a)) term (next a) next b)))

(define (cont-frac-iter n d k)
  (define (combiner cf x)
    (/ (n x) (+ (d x) cf)))
  (define (term i) (+ (- k i) 1))
  (define (next i) (+ i 1))
  (accumulate-iter combiner (/ (n k) (d k)) term 1 next (- k 1)))

(print (cont-frac-iter (lambda (i) 1.0)
		       (lambda (i) 1.0)
		       9))

(print (cont-frac-iter (lambda (i) 1.0)
		       (lambda (i) 1.0)
		       10))

(print (cont-frac-iter (lambda (i) 1.0)
		       (lambda (i) 1.0)
		       11))

(print (cont-frac-iter (lambda (i) 1.0)
		       (lambda (i) 1.0)
		       12))
