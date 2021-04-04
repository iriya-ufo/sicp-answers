;; ex2.10.scm

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p2 p2 p3 p4))))

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (div-interval x y)
  (if (< (* (upper-bound y) (lower-bound y)) 0)
      (error "Don't divide by an interval that spans zero.")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(define error-check (make-interval -1.0 2.0))
(lower-bound error-check)		; => -1.0
(upper-bound error-check)		; => 2.0
(div-interval error-check error-check)	; => *** ERROR: Don't divide by an interval that spans zero.
