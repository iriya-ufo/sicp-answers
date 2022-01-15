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

(define p1 (make-point 1 2))
(define p2 (make-point 6 2))
(define p3 (make-point 6 5))
(define p4 (make-point 1 5))

(define p1 (make-point -3 -8))
(define p2 (make-point -5 -8))
(define p3 (make-point -5 4))
(define p4 (make-point -3 4))

;; 周囲の長さ [perimeter]
(define (perimeter rec)
  (* 2 (+ (length rec) (width rec))))

(define length-seg (make-segment p1 p2))
(define width-seg (make-segment p1 p4))
(perimeter (make-rectangle length-seg width-seg))

;; 面積 [area]
(define (area rec)
  (* (length rec) (width rec)))

(area (make-rectangle length-seg width-seg))

;; 長方形 [rectangle] 表現
(define (make-rectangle length width) (cons length width))
(define (length rec) (abs (- (cadr length-seg) (caar length-seg))))
(define (width rec) (abs (- (cddr width-seg) (cdar width-seg))))
