;; ex1.39.scm

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

(define (tan-cf x k)
  (let ((tanx (cont-frac (lambda (i) (* -1 (* x x)))
			 (lambda (i) (- (* 2 i) 1))
			 k)))
    (* -1 (/ tanx x))))

(print (tan-cf (/ 3.14159265 4) 2))
(print (tan-cf (/ 3.14159265 4) 3))
(print (tan-cf (/ 3.14159265 4) 4))
(print (tan-cf (/ 3.14159265 4) 5))
