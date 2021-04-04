;; ex2.02.scm

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment x y) (cons x y))
(define (start-segment p) (car p))
(define (end-segment p) (cdr p))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
	(end (end-segment seg)))
    (make-point (/ (+ (x-point start) (x-point end)) 2)
		(/ (+ (y-point start) (y-point end)) 2))))

(define p1 (make-point 2 4))
(define p2 (make-point 2 0))
(print-point (midpoint-segment (make-segment p1 p2)))

(define p3 (make-point -4 2))
(define p4 (make-point -2 8))
(print-point (midpoint-segment (make-segment p3 p4)))
