(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (midpoint-segment seg)
  (let ((start-p (start-segment seg))
        (end-p (end-segment seg)))
    (make-point (/ (+ (x-point start-p) (x-point end-p)) 2)
                (/ (+ (y-point start-p) (y-point end-p)) 2))))

;; test p1 p2
(define p1 (make-point 0 0))
(define p2 (make-point 1 1))
(define seg1 (make-segment p1 p2))
(print-point (midpoint-segment seg1))

;; test p3 p4
(define p3 (make-point 1 1))
(define p4 (make-point 3 5))
(define seg2 (make-segment p3 p4))
(print-point (midpoint-segment seg2))
