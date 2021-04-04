;; ex2.03.scm

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

(define p1 (make-point 0 0))
(define p2 (make-point 4 0))
(print-point (midpoint-segment (make-segment p1 p2)))

(define p3 (make-point 4 10))
(define p4 (make-point 0 10))
(print-point (midpoint-segment (make-segment p3 p4)))


;; 長方形 [rectangle] 表現
(define (make-rectangle base height) (cons base height))
(define (base rec) (- (cadr base-seg) (caar base-seg)))
(define (height rec) (- (cddr height-seg) (caar height-seg)))

;; 周囲の長さ [perimeter]
(define (perimeter rec)
  (* 2 (+ (base rec) (height rec))))

(define base-seg (make-segment p1 p2))
(define height-seg (make-segment p1 p4))
(print (perimeter (make-rectangle base-seg height-seg)))

;; 面積 [area]
(define (area rec)
  (* (base rec) (height rec)))

(print (area (make-rectangle base-seg height-seg)))
